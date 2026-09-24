# Media sentiment comparison: Qwen and Jev

This directory contains two independent sentiment-classification pipelines and an R analysis script that compares their outputs.

The analysis is descriptive. It shows how the two classifiers label this media archive; it does not establish that sentiment causes market returns or that either classifier measures investor sentiment correctly.

## What was classified

The scripts combine two sources.

### Video

`/mnt/data/recoll/MEDIA.db` contains the `META` table. The relevant fields are:

- `ID`: YouTube video ID
- `SRC`: source/channel name, such as `CNBC-TV18`, `NDTV`, or `ET-NOW`
- `UPLOAD_DT`: upload date in `YYYYMMDD` form
- `TITLE`: video title

The transcript is read from:

```text
/mnt/ssd1/stockviz/lob/youtube/<SRC>/audio/<ID>.txt
```

The classifier state contains the title and transcript. If a transcript is not available, the title is still used.

The saved result uses:

```text
SOURCE_TYPE = VIDEO
ITEM_ID     = YouTube ID
SOURCE      = MEDIA.META.SRC
DATE_STAMP  = upload date converted to YYYY-MM-DD
```

### Print

`/mnt/data/recoll/NEWS.db` contains the `ARTICLES` table. The relevant fields are:

- `URL`: article URL and table primary key
- `TITLE`: article title
- `CONTENT`: article text stored in the database
- `PUB_DT`: publication date in `YYYYMMDD` form

The classifier state contains the title and article content.

The saved result uses:

```text
SOURCE_TYPE = PRINT
ITEM_ID     = ARTICLES.URL
SOURCE      = short source label derived from the URL
DATE_STAMP  = publication date converted to YYYY-MM-DD
```

For example, a Livemint URL is stored with `SOURCE = livemint`, while the full URL remains in `ITEM_ID`.

## Sentiment labels

Every classification is normalized to one of three labels:

- `POSITIVE`: predominantly favorable, optimistic, or positive language
- `NEGATIVE`: predominantly unfavorable, pessimistic, or negative language
- `NEUTRAL`: mainly factual, balanced, mixed, or without a clear overall direction

These are labels for the tone of the article or video, not recommendations about the security, sector, company, or market discussed in it. A positive article about a company in financial trouble is still classified according to the article's overall language and framing.

## Qwen pipeline

Script:

```text
qwen/classify_sentiment.py
```

Model and serving stack:

- `Qwen/Qwen2.5-0.5B-Instruct`
- GGUF `Q5_K_M` quantization from Hugging Face
- `llama.cpp` server in Docker
- CPU-only inference
- OpenAI-compatible endpoint at `http://localhost:8080`

The Qwen prompt asks for one sentiment label and explicitly says to judge the article's overall tone rather than whether the subject is a good or bad investment.

The Qwen context is deliberately bounded. The default article state is limited to 6,000 characters. If llama.cpp rejects a request because it exceeds the 4,096-token server context, the script retries with 4,000 and then 2,500 characters. The script also normalizes abbreviated generations such as `POS`, `NEG`, and `NEU`.

Results are stored in:

```text
qwen/SENTIMENT.db
```

## Jev pipeline

Script:

```text
jev/classify_sentiment.py
```

Jev is called through the TypeSafe System One API:

- Model: `jev-latest`
- Endpoint: `https://api.typesafe.ai/v1/systemone`
- Primitive: typed `Choice`
- Options: `POSITIVE`, `NEGATIVE`, `NEUTRAL`

The API key is read at runtime from:

```text
jev/jev.key
```

The key is not part of the result database or the analysis artifacts and should not be committed to source control.

The Jev question provides an explicit criterion for each of the three options, including a separate definition for neutral, factual, balanced, or mixed articles.

Results are stored independently in:

```text
jev/SENTIMENT.db
```

The two pipelines are intentionally siloed. Each reads the source databases read-only and writes only to its own result database.

## Resuming and database connections

Both classifiers read the selected `MEDIA.db` or `NEWS.db` table into memory and close the source SQLite connection before sending any inference requests.

The result database is opened separately. At startup, the script loads the existing `(SOURCE_TYPE, ITEM_ID)` keys. Completed items are skipped, and every new result is committed immediately after classification. A stopped run therefore resumes from the next unclassified item instead of submitting completed items again.

Use `--overwrite` only when deliberately replacing existing classifications.

Examples:

```bash
cd /mnt/data/blog/media-sentiment
python3 qwen/classify_sentiment.py
python3 jev/classify_sentiment.py
```

Useful controls include:

```bash
python3 qwen/classify_sentiment.py --source-type VIDEO --limit 100
python3 jev/classify_sentiment.py --source-type PRINT --limit 100
```

## Analysis artifacts

The comparison script is:

```text
analyze_sentiment.R
```

Run it with:

```bash
cd /mnt/data/blog/media-sentiment
Rscript analyze_sentiment.R
```

It reads both result databases and writes to:

```text
sentiment-analysis/
```

### 1. Aggregate sentiment

```text
01_aggregate_qwen_vs_jev.png
aggregate_sentiment.csv
```

This compares the total positive, neutral, and negative shares for the two approaches. Each model's percentages are normalized by that model's own number of classified records.

Current archive totals:

| Model | Records | Positive | Neutral | Negative |
|---|---:|---:|---:|---:|
| Qwen | 50,298 | 65.0% | 5.3% | 29.7% |
| Jev | 50,327 | 37.2% | 44.1% | 18.6% |

### 2. Sentiment by source type

```text
02_sentiment_by_source_type.png
sentiment_by_source_type.csv
```

The Qwen and Jev panels are stacked vertically. Percentages are calculated separately within each model and source type.

Current results:

| Model | Type | Positive | Neutral | Negative |
|---|---|---:|---:|---:|
| Qwen | Video | 61.8% | 6.6% | 31.6% |
| Qwen | Print | 75.8% | 1.0% | 23.2% |
| Jev | Video | 35.5% | 42.9% | 21.6% |
| Jev | Print | 43.0% | 48.3% | 8.7% |

### 3. Sentiment by source

```text
03_sentiment_by_source.png
sentiment_by_source.csv
```

The source panels are also stacked vertically. Each source's percentages sum to 100% within a model.

The source labels currently include:

- `CNBC-TV18`
- `NDTV`
- `ET-NOW`
- `economictimes`
- `livemint`
- `thehindubusinessline`

### 4. Daily positive sentiment versus market returns

```text
04_daily_positive_sentiment_vs_index_returns.png
daily_positive_sentiment_vs_index_returns.csv
```

The analysis downloads daily total-return index observations into:

```text
index_returns.csv
```

The three indices are:

- NIFTY 50 TR
- NIFTY MIDCAP 150 TR
- NIFTY SMALLCAP 250 TR

For each model and date, daily positive sentiment is:

```text
100 * positive classifications on the date / all classifications on the date
```

That daily percentage is joined to the same-date index return. The chart has Qwen on top and Jev below, with one facet per index and an ordinary least-squares fit in each facet.

This is a same-date association. It is not a next-day return test, and it does not control for market regime, news volume, day of week, publication mix, or autocorrelation.

## Additional diagnostic artifacts

The analysis now produces a paired-comparison and market-diagnostic set in addition to the first four charts. Paired charts use the 50,296 identical `(SOURCE_TYPE, ITEM_ID)` keys present in both result databases. This avoids confusing model disagreement with differences in coverage.

### Paired model comparison

```text
05_paired_confusion_matrix.png
paired_confusion_matrix.csv
paired_agreement_summary.csv
```

The confusion matrix places Qwen labels on the rows and Jev labels on the columns. Cell percentages are calculated within each Qwen row. The summary reports the paired sample size, raw agreement, and Cohen's kappa.

```text
06_qwen_minus_jev_by_source_type.png
07_qwen_minus_jev_by_source.png
qwen_minus_jev_by_source_type.csv
qwen_minus_jev_by_source.csv
```

These diverging bars show the Qwen percentage minus the Jev percentage for each sentiment class. Positive values mean Qwen assigns that class more often among the same paired records.

```text
14_disagreement_categories.png
disagreement_categories_by_source_type.csv
```

This breaks disagreement into exact label pairs, such as `Qwen POSITIVE vs Jev NEUTRAL`, separately for video and print.

### Time, source, and coverage diagnostics

```text
08_daily_sentiment_and_volume.png
daily_sentiment_series.csv
09_daily_disagreement_and_volume.png
daily_model_disagreement.csv
10_source_sentiment_heatmap.png
paired_source_sentiment_heatmap.csv
11_paired_coverage_by_source.png
paired_coverage_by_source.csv
12_sentiment_entropy_by_source.png
sentiment_entropy_by_source.csv
13_sentiment_vs_daily_volume.png
```

These show the evolution of the label mix, the daily disagreement rate, source-level distributions, paired coverage, entropy, and the relationship between daily article volume and sentiment share. Charts 08 and 09 use a minimum threshold of 50 records only to mark their starting date; they do not remove later dates with fewer records. Chart 08 starts on July 5, 2024, when the combined Qwen/Jev daily archive first reaches that threshold. Chart 09 starts on January 27, 2025, when at least 50 paired records are first available on a date. Every later date is retained in the chart and its CSV, including low-volume dates.

Entropy is measured in bits. A low value means that a model concentrates on one or two labels for that source; a higher value means the three labels are used more evenly. It is a description of model output behavior, not a measure of correctness.

### Market-return diagnostics

```text
15_positive_sentiment_by_return_bucket.png
positive_sentiment_by_return_bucket.csv
16_lagged_sentiment_return_coefficients.png
lagged_sentiment_return_coefficients.csv
17_sentiment_event_study.png
sentiment_event_study.csv
```

The return-bucket chart compares average positive sentiment across daily return quintiles. The lagged chart fits separate OLS regressions for same-day, next-day, two-day, and five-trading-day forward returns. Its coefficient is expressed in basis points of return per one percentage-point increase in positive sentiment, with a 95% confidence interval.

The event study defines high-positive and high-negative events as model-specific top-decile days for the corresponding daily sentiment share. It then shows average index returns from five trading days before through ten trading days after each event. Event-study observations are descriptive and are not an event-trading strategy.

The charts use the viridis family of palettes for categorical and continuous encodings. This keeps the figures readable in grayscale and more accessible to readers with common forms of color-vision deficiency.

## New findings from the diagnostic charts

### Agreement is limited, and the disagreement is directional

The paired sample contains 50,296 records. Exact Qwen–Jev agreement is 43.0%, with Cohen's kappa of 0.161. That is weak agreement beyond chance, so the two scores should not be treated as interchangeable measurements of the same calibrated variable.

The most common disagreement is Qwen `POSITIVE` versus Jev `NEUTRAL`:

- Video: 25.1% of paired records.
- Print: 32.4% of paired records.

The next large disagreement is Qwen `NEGATIVE` versus Jev `NEUTRAL`:

- Video: 14.7%.
- Print: 15.3%.

This is consistent with Jev using neutral as a broad middle category while Qwen makes a directional choice. Direct positive-versus-negative reversals are much less common for print than for video, but they still matter for video: Qwen positive versus Jev negative is 9.4% of paired video records.

### The model gap survives pairing

On the paired sample, Qwen assigns:

- Video: 61.8% positive, 6.6% neutral, 31.6% negative.
- Print: 75.8% positive, 1.0% neutral, 23.2% negative.

Jev assigns:

- Video: 35.5% positive, 42.9% neutral, 21.6% negative.
- Print: 43.1% positive, 48.3% neutral, 8.6% negative.

The Qwen-minus-Jev differences are therefore not caused only by the two pipelines processing different items. Among identical items, Qwen has 26.3 percentage points more positive video labels and 32.7 points more positive print labels. Jev has 36.3 points more neutral video labels and 47.3 points more neutral print labels.

The source-level comparison has the same pattern. For example, the paired Livemint records differ by 31.1 points in positive share, 59.9 points in neutral share in the opposite direction, and 28.8 points in negative share. Thehindubusinessline has the largest Qwen positive share at 88.5%, but that remains a model-output statistic rather than evidence that the publisher is intrinsically bullish.

### Source entropy confirms different label policies

Jev has higher sentiment entropy than Qwen for every listed source. For example:

- CNBC-TV18: Jev 1.53 bits versus Qwen 1.17 bits.
- NDTV: Jev 1.54 versus Qwen 1.26.
- thehindubusinessline: Jev 1.23 versus Qwen 0.54.

Qwen's lower entropy is driven by its very small neutral bucket and concentration in positive or negative labels. Jev distributes more records across all three classes. This supports the interpretation that the models differ in decision threshold and neutral usage, not merely in average sentiment level.

### Daily disagreement is substantial

Across 1,609 dates with paired records, the mean daily disagreement rate is approximately 54.1% and the median is approximately 55.6%. Some low-volume dates reach 100% disagreement, which is why the paired-volume panel is shown underneath the disagreement series. Daily disagreement should be interpreted together with coverage: a percentage based on one or two items is unstable.

### The return charts do not show a robust predictive relationship

The return-quintile chart does not show a monotonic relationship between positive sentiment and same-day returns. Qwen positive share varies within a relatively narrow high range across return buckets, while Jev varies within a narrower middle range. The largest visible differences are small compared with the model-level calibration gap.

The lagged regressions are similarly weak. Across all models, indices, and horizons, the R-squared values are close to zero. The largest absolute estimated slope is less than 0.20 basis points of return per one percentage-point increase in positive sentiment, and its confidence interval includes zero. These regressions do not establish a useful same-day or forward-return signal.

The event study is useful for checking whether extreme sentiment days line up with visibly unusual subsequent market paths. The current event curves are noisy and confidence bands are wide. They should not be interpreted as evidence that high-positive or high-negative sentiment causes returns, particularly because events are selected using the same archive being evaluated and no out-of-sample split is applied.

### Caveats for all new charts

- The charts measure classifier outputs, not human-validated sentiment.
- Paired charts remove coverage differences but do not remove model calibration differences.
- Daily aggregation mixes sources, publication times, topics, and article counts.
- Same-date returns are not necessarily available to an investor when a media item was published.
- Return-bucket and event-study results are exploratory and in-sample.
- No multiple-testing correction is applied across the many diagnostics.
- The event study uses top-decile thresholds and should be treated as descriptive rather than confirmatory.

## Interpretation

### The aggregate result

The biggest difference is not a small shift at the margin. Qwen calls nearly two-thirds of the archive positive, while Jev calls fewer than two-fifths positive and labels almost half of the archive neutral.

Qwen's distribution is strongly polarized: 65.0% positive and 29.7% negative, with only 5.3% neutral. Jev is much more conservative about assigning direction: 37.2% positive, 18.6% negative, and 44.1% neutral.

The first conclusion is therefore about calibration and label policy, not about which model is "right." Qwen is making a directional call on most articles. Jev is reserving directional labels for articles that more clearly support them.

The negative share does not move in the same direction as the positive share. Qwen is more negative than Jev overall, even though it is also much more positive. That pattern is consistent with a generative classifier that tends to choose one of the two polar labels, while Jev uses neutral as a genuine third outcome.

### Video versus print

Qwen is more positive on both source types, but the difference is larger for print:

- Qwen: 75.8% positive for print and 61.8% for video.
- Jev: 43.0% positive for print and 35.5% for video.

Jev assigns neutral to 48.3% of print and 42.9% of video. This is not simply a weaker version of Qwen's output. It is a different partition of the same material.

Print articles often contain earnings previews, analyst expectations, management guidance, and balanced lists of risks and opportunities. Jev's neutral criterion appears to treat that reporting style as factual or mixed. Qwen more often converts the same favorable elements into a positive label, even when the article also contains caveats.

Video transcripts are less polished and more conversational. They also contain interviews, commentary, predictions, and repeated promotional phrasing. Both models produce more negative video labels than print labels, but Jev still leaves a large share of video neutral.

### Differences by source

The model gap is visible across almost every source, so it is not explained by one publisher alone.

Examples:

- `CNBC-TV18`: Qwen is 62.1% positive; Jev is 34.6% positive and 43.9% neutral.
- `NDTV`: Qwen is 61.2% positive; Jev is 35.0% positive and 41.8% neutral.
- `livemint`: Qwen is 55.3% positive and 42.3% negative; Jev is 24.2% positive, 13.5% negative, and 62.3% neutral.
- `thehindubusinessline`: Qwen is 88.5% positive; Jev is 56.0% positive and 38.3% neutral.
- `economictimes`: Qwen is 79.0% positive; Jev is 42.8% positive and 49.5% neutral.

The Livemint result is especially useful. Qwen sees a strongly directional source, split between positive and negative. Jev sees mostly neutral coverage. That suggests the models are reacting differently to financial journalism's standard mix of facts, forecasts, risks, and quoted opinions.

The BusinessLine result also needs caution. A high positive share can reflect the subject mix, the period covered, or the way headlines and transcripts are written. It should not be read as evidence that BusinessLine is generally bullish without controlling for the underlying articles.

Source counts are not perfectly identical between the two databases. The pipelines were run separately and did not finish with exactly the same records: Qwen has 50,298 saved classifications and Jev has 50,327. Small count differences are therefore a coverage issue as well as a model issue. A strict paired comparison should be run on the intersection of identical `(SOURCE_TYPE, ITEM_ID)` keys.

### Daily positive sentiment and index returns

The scatterplot is useful mainly as a sanity check. The current same-date correlations are small:

| Model | Index | Observations | Correlation | R-squared |
|---|---|---:|---:|---:|
| Qwen | NIFTY 50 TR | 1,286 | 0.040 | 0.002 |
| Qwen | NIFTY MIDCAP 150 TR | 1,286 | 0.051 | 0.003 |
| Qwen | NIFTY SMALLCAP 250 TR | 1,286 | 0.051 | 0.003 |
| Jev | NIFTY 50 TR | 1,286 | 0.025 | 0.001 |
| Jev | NIFTY MIDCAP 150 TR | 1,286 | 0.032 | 0.001 |
| Jev | NIFTY SMALLCAP 250 TR | 1,286 | 0.039 | 0.002 |

The fitted lines are nearly flat in practical terms. The plots do not show a meaningful same-day linear relationship between the fraction of positive media items and the return of these indices.

That result should not be overinterpreted. The test uses only days with classified media, aggregates articles from different sources, and compares the final daily label share with the return from the same date. It does not test whether sentiment leads returns, whether sentiment predicts abnormal returns, or whether a source-specific signal works after publication-time alignment. A useful follow-up would use publication timestamps, lagged sentiment, article counts, source fixed effects, and out-of-sample evaluation.

## Why Qwen and Jev differ

### 1. They are different kinds of models

Qwen is a small instruction-tuned language model used as a text generator. The script asks it to generate a label, then parses the generated text. It sometimes produces abbreviated labels such as `NEG`, which the script normalizes.

Jev is a structured decision model. The script sends a typed `Choice` question and receives a selected option with probabilities and confidence in the API response. There is no free-form label parsing in the Jev path.

A generative model is being asked to imitate a classifier. Jev is being asked to make a typed choice. That difference alone can change how often the neutral class is used.

### 2. The prompts define neutral differently

The Qwen prompt asks for the article's overall tone and gives the three labels in one instruction. It does not provide a long, separate rubric for each option.

The Jev request defines each class explicitly:

- Positive means predominantly favorable or optimistic.
- Negative means predominantly unfavorable or pessimistic.
- Neutral means factual, balanced, mixed, or without a clear direction.

Jev therefore has a stronger operational definition of neutral. Qwen has more room to infer what the labels mean from the general instruction and the text itself.

### 3. Qwen sees less text on long items

Qwen uses a 6,000-character default limit and may fall back to 4,000 or 2,500 characters when the local server rejects a request. Jev uses a 12,000-character default limit.

For long videos and articles, Qwen may see the title plus an early portion of the transcript or content. Jev may see more of the later discussion, including qualifications, risks, or opposing views. Truncation can push the models toward different labels even when their general language judgment is similar.

### 4. The Qwen model is very small

Qwen2.5-0.5B is useful for a local CPU pipeline, but it has far less capacity than a large hosted decision model. It is more likely to rely on obvious lexical cues such as "growth," "strong," "surge," "crisis," or "decline." Financial articles often contain both positive and negative cues, so shortcutting can produce a polar label where a larger structured model chooses neutral.

### 5. The model outputs are not calibrated against each other

A `POSITIVE` from Qwen and a `POSITIVE` from Jev are nominally the same class, but they are not guaranteed to have the same threshold. Neither pipeline was calibrated against a human-labeled validation set in this project.

The output proportions show why raw model agreement should not be assumed. Before using these scores as a research feature, create a manually labeled sample, measure precision and recall by class, and decide whether neutral should be treated as a separate state or merged with weak sentiment.

### 6. The databases are not perfectly paired

The two runs were resumable and independent. A failed request, a newly ingested article, or a different stopping point changes the set of records in each result database. This affects aggregate proportions and source counts.

For a clean model comparison, use an inner join on:

```text
SOURCE_TYPE + ITEM_ID
```

Then report the paired confusion matrix, agreement rate, and disagreement rate. The current charts compare each model's available archive, which is useful for operational monitoring but is not a fully paired evaluation.

## Practical conclusions

1. Qwen currently behaves like a directional, high-polarity classifier. It produces very few neutral labels.
2. Jev currently behaves like a more conservative classifier with a large neutral bucket.
3. The difference is consistent across video, print, and most sources, so it is mainly methodological rather than a single-source anomaly.
4. Aggregate positive sentiment should not be compared between models without either calibration or a paired human-labeled benchmark.
5. The daily return scatterplots show no useful same-day linear relationship in this archive. They should not be treated as evidence for or against a predictive trading signal.
6. The next serious validation step is a manually labeled, identical article/video sample scored by both systems, followed by a paired confusion matrix and out-of-sample test.

## References

- Qwen model: https://huggingface.co/Qwen/Qwen2.5-0.5B-Instruct
- Qwen GGUF repository: https://huggingface.co/Qwen/Qwen2.5-0.5B-Instruct-GGUF
- TypeSafe Jev introduction: https://docs.typesafe.ai/introduction
- TypeSafe quick start and API format: https://docs.typesafe.ai/introduction/quickstart
- TypeSafe Choice primitive: https://docs.typesafe.ai/primitives/choice
- Nifty Indices historical total-return data: https://www.niftyindices.com/reports/historical-data
