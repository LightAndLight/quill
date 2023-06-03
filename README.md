# `quill`

*2023-06-03*

Quill was a database query prototype that I created in 2020. [Laurel](https://github.com/LightAndLight/laurel#readme) is my current iteration of database-y query language-y research.

Some things I was playing with:

* Programming language improvements (<https://github.com/LightAndLight/quill/blob/3532e15e4a3c7135486f706159b0841daffed153/NOTES.md#database>)
  * Simpler / more modern syntax
  * Type checking
  * Monadic sequencing of such as `insert`ing into tables (I currently don't endorse this)
  
* Improving migrations (<https://github.com/LightAndLight/quill/blob/master/DESIGN.md#migrations>)
  * Consistency and type checking
  * View the current schema implied by a set of migrations
  
* Client / server "plugin" system to support multiple backends ([protocol definitions](https://github.com/LightAndLight/quill/tree/master/quill-backend-api/schemas)) (not sure whether I endorse this any more)

Things I was aiming for but didn't implement:

* Compile "quill" query files to language-specific code (<https://github.com/LightAndLight/quill/projects/2#card-67470085>, <https://github.com/LightAndLight/quill/projects/2#card-36183490>)
* Newtypes (<https://github.com/LightAndLight/quill/projects/2#card-36183417>)
* Informative migration errors (<https://github.com/LightAndLight/quill/projects/2#card-36235067>)