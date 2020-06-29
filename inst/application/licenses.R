# . license_tab ----
license_tab <- tabItem(
  tabName = "license_tab",
  h2("Common licenses in order of most open to most restrictive"),
  HTML("From <a href='https://help.data.world/hc/en-us/articles/115006114287-Common-license-types-for-datasets' target='_blank'>data.world</a>"),

  h3("Public Domain"),
  HTML("<a href='' target-'_blank'>Public Domain Mark</a>"),
  p("Dedicate your dataset to the public domain: This isn’t technically a license since you are relinquishing all your rights in your dataset by choosing to dedicate your dataset to the public domain. To donate your work to the public domain, you can select “public domain” from the license menu when creating your dataset."),

  h3("CC-0"),
  HTML("<a href='' target-'_blank'>Creative Commons Public Domain Dedication</a>"),
  p("This license is one of the open Creative Commons licenses and is like a public domain dedication. It allows you, as a dataset owner, to use a license mechanism to surrender your rights in a dataset when you might not otherwise be able to dedicate your dataset to the public domain under applicable law."),

  h3("PDDL"),
  HTML("<a href='' target-'_blank'>Open Data Commons Public Domain Dedication and License</a>"),
  p("This license is one of the Open Data Commons licenses and is like a public domain dedication. It allows you, as a dataset owner, to use a license mechanism to surrender your rights in a dataset when you might not otherwise be able to dedicate your dataset to the public domain under applicable law."),

  h3("CC-BY"),
  HTML("<a href='' target-'_blank'>Creative Commons Attribution 4.0 International</a>"),
  p("This license is one of the open Creative Commons licenses and allows users to share and adapt your dataset so long as they give credit to you."),

  h3("CDLA-Permissive-1.0"),
  HTML("<a href='' target-'_blank'>Community Data License Agreement – Permissive, Version 1.0</a>"),
  p("This license is one of the Community Data License Agreement licenses and is similar to permissive open source licenses. It allows users to use, modify and adapt your dataset and the data within it, and to share it so long as they give credit to you. The CDLA-Permissive terms explicitly do not impose any obligations or restrictions on results obtained from users’ computational use of the data."),

  h3("ODC-BY"),
  HTML("<a href='' target-'_blank'>Open Data Commons Attribution License</a>"),
  p("This license is one of the Open Data Commons licenses and allows users to share and adapt your dataset so long as they give credit to you."),

  h3("CC-BY-SA"),
HTML("<a href='' target-'_blank'>Creative Commons Attribution-ShareAlike 4.0 International</a>"),
  p("This license is one of the open Creative Commons licenses and allows users to share and adapt your dataset so long as they give credit to you and distribute any additions, transformations or changes to your dataset under this license. We consider this license (a.k.a a viral license) problematic since others may decide not to work with your CC-BY-SA licensed dataset if there is risk that by doing so their work on your dataset will need to be shared under this license when they would rather use another license."),

  h3("CDLA-Sharing-1.0"),
  HTML("<a href='' target-'_blank'>Community Data License Agreement – Sharing, Version 1.0</a>"),
  p("This license is one of the Community Data License Agreement licenses and was designed to embody the principles of &ldquo;copyleft&rdquo; in a data license. It allows users to use, modify and adapt your dataset and the data within it, and to share the dataset and data with their changes so long as they do so under the CDLA-Sharing and give credit to you. The CDLA-Sharing terms explicitly do not impose any obligations or restrictions on results obtained from users’ computational use of the data."),

  h3("ODC-ODbL"),
HTML("<a href='' target-'_blank'>Open Data Commons Open Database License</a>"),
  p("This license is one of the Open Data Commons licenses and allows users to share and adapt your dataset so long as they give credit to you and distribute any additions, transformation or changes to your dataset under this license. We consider this license (a.k.a a viral license) problematic since others may decide not to work with your ODC-ODbL licensed dataset if there is risk that by doing so their work on your dataset will need to be shared under this license when they would rather use another license."),

    h3("CC BY-NC"),
  HTML("<a href='' target-'_blank'>Creative Commons Attribution-NonCommercial 4.0 International</a>"),
  p("This license is one of the more restrictive Creative Commons licenses. Users can share and adapt your dataset if they give credit to you and do not use your dataset for any commercial purposes."),

  h3("CC BY-ND"),
HTML("<a href='' target-'_blank'>Creative Commons Attribution-NoDerivatives 4.0 International</a>"),
  p("This license is one of the more restrictive Creative Commons licenses. Users can share your dataset if they give credit to you, but they cannot make any additions, transformations or changes to your dataset under this license."),

    h3("CC BY-NC-SA"),
  HTML("<a href='' target-'_blank'>Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International</a>"),
  p("This license is one of the most restrictive Creative Commons licenses. Users can share your dataset only if they (1) give credit to you, (2) do not use your dataset for any commercial purposes, and (3) distribute any additions, transformations or changes to your dataset under this license. We consider this license a viral license since users will need to share their work on your dataset under this same license and any users of the adapted dataset would likewise need to share their work on the adapted dataset under this license and so on for any other changes to those modified datasets."),

  h3("CC BY-NC-ND"),
  HTML("<a href='' target-'_blank'>Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International</a>"),
  p("This license is one of the most restrictive Creative Commons licenses. Users can share only your unmodified dataset if they give credit to you and do not share it for commercial purposes. Users cannot make any additions, transformations or changes to your dataset under this license.")
)
