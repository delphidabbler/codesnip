# Database Structure Design

## Overview

CodeSnip maintains a database comprising snippets from two data sources:

1. The _"main"_ data source -- Read only snippets read from data downloaded from the [DelphiDabbler Code Snippets Collection](https://github.com/delphidabbler/code-snippets).

2. The _"user"_ data source -- Snippets created by the user that can be created, edited and deleted from within CodeSnip. These snippets are stored on the user's computer in a special directory. It is possible to import snippets exported from another user's snippets collection or from the [SWAG Archive](https://github.com/delphidabbler/swag). Such imported snippets become part of the user's own "user" data source and retain no knowledge of where they originally came from.

The database also contains various category records. Categories read from the main data source can't be edited. Categories created by the user are stored in the user data source. Snippets from the user data source can reference categories from the main data source, but the reverse never happens.

Throughout much of CodeSnip's documentation, the "main" and "user" data sources will be referred to as the "main" and "user" databases. However, there is only one database that has two data sources: "main" and "user".

## Tables

Internally, CodeSnip maintains two in-memory tables:

1.  The _snippets_ table -- Contains a record for each snippet
2.  The _categories_ table -- Contains a record for each category

Both Snippet and Category records identify which data source they belong to by means of a `UserDefined` field. This is true for the "user" data source and false for the "main" data source.

> ⭐ When adding further data sources we will need to change the `UserDefined` fields to something like `DataSource` or `Origin` so we know which data source the snippet belongs to.

> ⭐ When creating snippets we will need to get the user to specify a data source for. Since it's very likely that a snippet will be mis-allocated we will also need to provide a way to move snippets between data sources.

> ⭐ For as long as the "main" data source is special, we will need a property or method for snippets like `IsEditable` or `IsMain` that can be used to prevent editing a snippet.

