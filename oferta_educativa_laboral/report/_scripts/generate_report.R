

library('quarto')
quarto::quarto_render(
    input = "report_template.qmd"
    ) #,
    # output_format = 'html',
    # output_file = "",
    # execute_params = list( # f using r 'params' style
    #     name = ,
    #     file =
    # ),
    # )



# From the CLI html is default:
# quarto render report.qmd -P name:some_name -P file:some_file
# quarto render report.qmd -P name:some_name -P file:some_file --to pdf

# CLI parameters to change metadata with '-M' option:
# quarto render report.qmd -P name:some_name -P file:some_file -M toc:true
# (creates a sidebar TOC in html)
# quarto render report.qmd -P name:some_name -P file:some_file -M toc:true -M "title:some other title"

