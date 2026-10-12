# Methods and interpretation

## Collection

The saved study contains nine videos and 3,600 comment records per artist. Searches combined artist names with official music video, lyric video and official audio, with an additional live-performance query for Billie Eilish. Taylor/Ariana queries requested up to 10 videos; Billie's core queries requested 50 and the live query 20. Official artist/VEVO channels were restricted or filtered, candidate videos were deduplicated, and a publication window from 1 January 2024 to before 23 August 2026 was used.

English relevance was a search preference, not a language guarantee for comments. The retained sample mixes release formats. The 400-record cap supported equal-sized comparisons but discarded extra available records. Parent/thread identifiers were not retained in the final clean schema, so complete reply coverage cannot be established from that file. A separate revised collector is included as future work; its results are not substituted for the published dataset.

The initial collection had ten blank texts across eight videos. Those videos were retrieved again, valid text was selected and the same 400-record cap was applied. The final snapshot has 10,800 unique comment IDs and no blank texts. Validation checks distinguish successful cleaning from completeness or representativeness.

## Audience participation

An actor is a distinct commenter identified by `author_channel_id`. Comment counts are first grouped by artist and actor. An actor with one retained record is a single-comment actor; one with more than one is a repeat actor.

`repeat share = repeat actors / unique actors × 100`.

Taylor has 2,657 unique authors and 331 repeat authors (12.46%); Ariana has 2,932 and 283 (9.65%); Billie has 3,174 and 272 (8.57%). Counting distinct IDs across the combined data gives 8,713 people. Forty-four appear in two artist samples and three in all three. These are identifiers within the snapshot, not verified identities, fan membership or loyalty measures.

## Actor networks and centrality

The actor graphs are undirected, weighted bipartite networks. One node type is the author and the other is a selected video. An edge means that an author commented on that video; its weight is the number of retained comments. The graphs do not establish friendship, direct replies, follower relationships or communication between authors.

- Degree counts videos connected to an author; weighted degree counts retained comments.
- PageRank identifies structural prominence under this graph definition. It is not evidence of real-world influence.
- Betweenness measures shortest-path bridging. Comment-count strengths are converted into distances for path-based measures.
- Closeness describes access through the graph under the chosen weighted paths and normalisation. Scores depend on graph structure and weights; they are not a directly observed speed of communication.

All three full actor–video graphs are connected. Comparisons still depend on different numbers of authors, the same nine video hubs and concentration of posting activity. Taylor's maximum retained author comment count is 64, Ariana's 107 and Billie's 11. Large activity counts do not, by themselves, establish spam.

## Word counts and context

Each comment is treated as a document. The preprocessing handles URLs, case, punctuation, numbers, standard stop words and selected artist/platform/filler terms. The complete Q5 script includes the final filler-word correction. Separate term-document matrices are constructed for each artist. Removing words can leave a document empty; these records are counted rather than silently interpreted as meaningful text.

The exact retained token `love` occurred 451 times for Taylor, 533 for Ariana and 673 for Billie. Occurrences are not the same as documents or authors:

| Artist | Token occurrences | Comments containing the token | Distinct authors using it |
|---|---:|---:|---:|
| Taylor Swift | 451 | 384 | 315 |
| Ariana Grande | 533 | 360 | 336 |
| Billie Eilish | 673 | 464 | 448 |

Ariana has more occurrences than Taylor but fewer comments containing the word. Repeated uses within a comment therefore matter. In the inspected examples, three Ariana lyric-copy comments contributed 15 uses each: 45/533 = 8.44%. Six Billie lyric-copy comments contributed nine uses each: 54/673 = 8.02%. These are specific examples, not an exhaustive classification of every mention. `love` does not count inflections such as `loves` or `loved`.

Weighted directed bigram graphs model retained word pairs. A minimum pair frequency of two reduces one-off edges. PageRank then ranks structural connectivity, whereas a term-frequency ranking counts occurrences. Tokenisation, stop-word removal and the cutoff influence the graph; the retained adjacency should not automatically be read as an exact quotation from the original sentence.

## Community detection

Girvan–Newman is compared with Louvain on restricted networks containing authors who commented across at least two videos, plus the video nodes. This includes 110 active authors for Taylor, 90 for Ariana and 58 for Billie. These are not the full 2,657/2,932/3,174-author networks.

Girvan–Newman uses path distances derived from inverse comment counts. Louvain treats counts as connection strengths. Weighted modularity is evaluated for the resulting partitions. The partitions can differ substantially: for Taylor, seven Girvan–Newman communities versus five Louvain communities; Ariana has four versus five; Billie has five versus five. Community labels are arbitrary. Higher modularity under the selected model does not prove more meaningful or more loyal fan groups.

## Sentiment and emotions

`sentimentr` scores original comment text using its polarity model. The study classifies scores greater than 0.05 as positive, less than -0.05 as negative, and the inclusive interval between them as neutral. Taylor has 1,446 positive, 1,743 neutral and 411 negative comments; Ariana 1,317/1,808/475; Billie 1,338/1,862/400.

NRC emotion results obtained through `syuzhet` count lexicon matches. A word can contribute to several emotions, and a comment can contribute multiple matches. Counts per 1,000 comments are rates of lexical matches, not percentages of people experiencing an emotion. Song lyrics, sarcasm, negation, slang and multilingual text limit interpretation. No manually annotated sentiment validation or causal popularity conclusion is claimed.

## Decision tree

The target is the artist sample associated with a comment. A seeded stratified 80/20 split produces 8,640 training and 2,160 test records, with 720 test comments per artist. The model uses binary features for 250 frequent training-vocabulary terms of at least three characters. Test term columns are aligned with the training feature order.

The baseline uses `rpart` with complexity parameter 0.01, minimum split 30, minimum bucket 10 and maximum depth eight. The tuned configuration explores complexity values through five-fold cross-validation, with smaller leaf settings and a maximum depth of 20. The selected complexity parameter is 0.0005. Test accuracy is 35.46% for baseline and 38.89% for tuned; macro F1 is 0.2094 and 0.2928.

The test set is balanced, so a single-class predictor obtains 33.33%. The tuned tree predicts Taylor in 1,934/2,160 cases (89.54%), with poor recall for Ariana and Billie. The corrected confusion matrix and saved performance table are committed. The improvement is modest, and the model is not a reliable artist classifier.

Vocabulary selection is performed on the complete training split before cross-validation rather than within every fold. The split is by comment rather than by author or video, and similar/copied text can span train and test. A stronger evaluation would fit preprocessing inside each fold, deduplicate or group repeated text, and test on unseen authors/videos. These refinements are proposed, not completed.

## LDA topics

Three topics are fitted separately to each artist's filtered corpus. Taylor retains 2,609 documents, Ariana 2,625 and Billie 2,448. Empty documents after vocabulary filtering are excluded and reported. The current code captures document labels before filtering and maps posterior probabilities back to the correct comment IDs.

The dominant topic is the largest posterior assignment for a document. This differs from the mean posterior probability across documents. Mean topic probabilities close to one-third caution against treating the topics as sharply separated categories. Topic numbers in independent models are not equivalent across artists. Perplexities from different corpora/vocabularies cannot establish that Billie's model is "best"; any such comparison would need a common evaluation design.

## Manager-facing use

The study helps identify conversations to inspect: breadth versus repeated participation, copied lyrics versus personal reaction, and comment reactions versus automated tone. It supports exploratory review, not a general ranking of artist popularity, audience loyalty or marketing effectiveness.
