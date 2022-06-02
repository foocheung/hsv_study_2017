
tabItem(tabName = "item1B" ,
        
        box(
          width = 12,
          HTML('
                           <h1><center> This is the heading</h1></center>
                          <br>
                          <p>A simple Shiny app to help allocate batches.
                          File format must be tab delimited and have column names, example can be seen <a href="./pheno.txt">here</a>
                          
                          <PRE>
                          SOMAscan® 1.1k Assay
                          SOMAscan® 1.3k Assay
                          SOMAscan® 5k Assay
                          
                          </PRE>
                          <br>
                          <h4>How To</h4>
                          <p>Step1) Upload your tab delimited file
                          <p>Step2) Select the number of batches you are using from the slider "# of Batches:"
                          <p>Step3) press GO
                          <p>
                          A recommendation for practice is to put the variable of primary interest as the first of the list.
                          <p>
                          Output: 
                          "Sample distribution by plates can also be visualized (Figure 1). It shows that samples with different
                    characteristics were distributed across batches with only small variations. The small variation is largely due
                    to the trade off in block randomizing multiple variables. The last plot is the index of optimization steps
                    versus value of the objective function. The blue diamond indicate the starting point, and the red diamond
                    mark the final optimal setup. It is clear that final setup is more optimal than the starting setup."
                          <br>
                          This app uses functions from the R package 
                          <a href="http://bioconductor.org/packages/2.11/bioc/html/OSAT.html">OSAT</a>
                          
                          </p>')         
          
        )
)
