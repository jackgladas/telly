# telly
Analysis of watched TV and film data. This R script handles the `Transforming the Data` step described in [this blog post](https://jackgladas.medium.com/my-year-in-telly-2ef5429bf665).

### Film input schema:
* Date (the date on which I watched the film)
* Name (the name of the film - shorthand as I actually use the full IMDb name in the viz)
* Rating (how much I enjoyed the film, from 1 (hated it) to 5 (loved it)
* Cinema (“Yes” if I watched the film in the cinema, blank otherwise)
* First Watch (“Yes” if this is the first time I’d watched that film, as far as I can recall!)
* IMDb (the IMDb unique identifier for that film, taken manually from the IMDb page URL)
* With (who I watched the film with)

### Series input schema:
* Show (the name of the TV series)
* Season (the season of the series)
* Start Date (the date on which I started watching that season)
* End Date (the date on which I finished watching that season, or blank if not yet finished)
* IMDb (the IMDb unique identifier for that series)
* With
* Latest Episode (if I haven’t yet finished the season, the number of the episode I watched last)
