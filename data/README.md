# Saved data

`comments.csv` is the final cleaned 10,800-record snapshot. `videos.csv` records the 27 selected videos and their collection-time metadata. CSV row order is preserved so the existing seeded split and document mappings remain meaningful.

| Field | Meaning |
|---|---|
| comment_id | Stable public label replacing the original YouTube comment ID |
| artist | Taylor Swift, Ariana Grande or Billie Eilish |
| video_id | Public YouTube video identifier |
| video_title | Title returned when the video was selected |
| search_term | Query provenance retained for the selected video |
| comment_text | Original retained text; preprocessing happens in R |
| author_channel_id | Stable public actor label replacing a commenter channel ID |
| like_count | Likes on the comment at collection time |
| published_at | Comment publication timestamp |
| collection_date | Snapshot collection date |
| source | YouTube |

Actor labels are assigned in the original identifiers' sorted order and are shared across the three artist samples. This preserves distinct-author counts, cross-artist linkage and lexical tie ordering. No identifier lookup is published. Comment text is not rewritten, so free text may contain identifying details and can potentially be matched to a public comment.

The video-publication filter is separate from comment-publication dates. English search relevance is not an English-only comment filter. The 400-record cap is a property of this saved sample, not the number of all available comments or top-level threads.

The text belongs to its original authors. The source-code licence does not transfer ownership of third-party comments, song lyrics or referenced material.
