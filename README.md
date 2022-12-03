# CodeSnip Pavilion

> **Branch abandoned**
> 
> The `pavilion` branch, which has been moribund since May 2020, was officially abandoned on 2021-12-17.
> 
> The branch will remain in the repository for the time being in case some of its code can be salvaged for use in a later version of CodeSnip.
> 
> Anyone is welcome to take over development of this branch. If you do, please fork the repository. But _do not_ submit any changes back here. On the other hand pull requests for the main CodeSnip v4 code base are welcome.

## Rationale behind the `pavilion` branch

By January 2020 it was looking likely that [delphidabbler.com](https://delphidabbler.com) would be closing down† by the end of Q2 2020. Such a closure would have serious consequences for CodeSnip in that the web site provided web services that CodeSnip depended upon. Consequently there was a need to modify CodeSnip to remove its dependency on these web services.

Two possible approaches were identified:

1. Modify CodeSnip 4 to strip out the web services and provide an alternative means of downloading and updating the DelphiDabbler Code Snippets Database, or
2. Complete development of CodeSnip 5. One of the design goals of CodeSnip 5 was to play down the importance of the Code Snippets Database and a new way of obtaining and integrating the database had already been considered.

Since a lot of development had already been done on CodeSnip 5 in the stalled `pagoda` branch it was decided to investigate the feasibility of completing that work by June 2020. Given that all web services would need to be removed from the `pagoda` code, it was decided to create a new branch off `pagoda` for this work. That way, if delphidabbler.com was saved in its current form, the `pagoda` branch could be used for development once again, with its web service code intact. Therefore the `pavilion` branch was branched from `pagoda` on 2020-01-24, reflecting `pagoda` as it was on 2016-07-13. `pavilion` had the following goals:

1. to remove CodeSnip's dependency on the web services
2. to unify the online Code Snippets database with the user's own snippets in a single database.

## Progress with `pavilion`

Some work was completed in stripping out the web services code, along with some other work on the v5 code base inherited from `pagoda`.

However, by the end of January 2020 it was becoming apparent that there was not enough time to complete all the outstanding work on v5 **and** get it thoroughly tested before the proposed closure date for delphidabbler.com.

## Failure of `pavilion`

Because of the problems identified above, it was decided to concentrate on updating CodeSnip 4 by excising its web service dependencies. This course of action seemed to be the most likely to meet the deadline. Consequently all work on `pavilion` ceased. A new version of CodeSnip 4 was indeed released on time.

The original intention was to come back to `pavilion` once the new 4.x release was out of the way, possibly with the view to issuing a less ambitious v5 release, with some features held over to v6. This was not to be: while there _were_ a few commits to the `pavilion` branch in May 2020, they were only to update documentation. And there things rested.

A subsequent review of the `pavilion` code led to the conclusion that that were simply too many problems with it to push ahead. If there was to be a CodeSnip 5 it would mist likely be easier to start again. Reluctantly it was decided to abandon the branch.

## About `pagoda`

For more information about the `pagoda` branch (and its predecessor, `parsnip`) see the `pagoda` branch read-me.

----

† delphidabbler.com didn't, in the end, close. Instead the site moved to a new host. But the web services were closed down and, at the time of writing, are very unlikely to return.
