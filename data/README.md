# 데이터

원본 데이터는 저장소에 포함하지 않았습니다. 뉴스 본문은 언론사 저작물이고, 유튜브 댓글은 일반 이용자가 작성한 글이기 때문입니다.

| 파일명 | 내용 | 형식 |
|---|---|---|
| `news_results_final.csv` | '마약' 키워드 네이버 뉴스 1,926건 | 네이버 검색 API 필드(title, originallink, link, description, pubDate) + 본문 `text` |
| `news_results_fina_teen.csv` | 미성년자 관련 마약 뉴스 985건 | 위와 동일 |
| `youtube_comments.txt` | 마약 관련 영상 댓글 | 한 줄에 댓글 하나, 영상마다 `Comments for video ID: ...` 구분선 |
| `youtube_comments_movie.txt`, `youtube_comments_drama.txt` | 마약 소재 영화·드라마 영상 댓글 | 위와 동일 |
