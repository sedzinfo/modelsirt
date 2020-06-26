##########################################################################################
# IRT MODELS
##########################################################################################
#' @title IRT models
#' @import magrittr
#' @importFrom shiny shinyApp tagList navbarPage tabPanel titlePanel h4 sidebarPanel selectInput sliderInput textOutput mainPanel tabsetPanel withMathJax textInput inputPanel plotOutput renderText reactive renderPlot
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_segments layout
#' @importFrom grDevices contourLines
#' @importFrom graphics abline arrows layout lines
#' @importFrom stats pnorm
#' @importFrom utils install.packages
#' @keywords irt models
#' @export
#' @examples
#' modelsirt()
modelsirt<-function() {
  ##########################################################################################
  # RACH
  ##########################################################################################
  la_rach<-seq(from=0.05,to=0.6,by=0.1)
  ua_rach<-seq(from=0.05,to=0.6,by=0.1)
  thetas_rach<-seq(from=-6,to=6,by=0.1)
  deltas_rach<-seq(from=-6,to=6,by=0.1)
  alphas_rach<-seq(from=-6,to=6,by=0.1)
  N_rach=length(thetas_rach)
  p1_rach<-matrix(NA,nrow=N_rach,1)
  Pfun_rach<-function(la=0,ua=1,alpha=1,theta,delta,D=1) {
    e_func<-exp(-(D*alpha*(theta-delta)))
    renum<-ua-la
    result<-la+((renum)/(1+e_func))
    return(result)
  }
  ##########################################################################################
  # PARTIAL CREDIT
  ##########################################################################################
  thetas_pcm<-seq(from=-6,to=6,by=0.1)
  deltas_pcm<-seq(from=-6,to=6,by=0.1)
  alphas_pcm<-seq(from=-6,to=6,by=0.1)
  N_pcm=length(thetas_pcm)
  p1num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p2num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p3num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p4num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p5num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p2fun_pcm<-function(alpha,theta,delta1,D){
    z1<-D*alpha*(theta-delta1)
    return(exp(0+z1))
  }
  p3fun_pcm<-function(alpha,theta,delta1,delta2,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    return(exp(0+z1+z2))
  }
  p4fun_pcm<-function(alpha,theta,delta1,delta2,delta3,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    z3<-D*alpha*(theta-delta3)
    return(exp(0+z1+z2+z3))
  }
  p5fun_pcm<-function(alpha,theta,delta1,delta2,delta3,delta4,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    z3<-D*alpha*(theta-delta3)
    z4<-D*alpha*(theta-delta4)
    return(exp(0+z1+z2+z3+z4))
  }
  ##########################################################################################
  # GRADED RESPONSE
  ##########################################################################################
  thetas_grm<-seq(from=-6,to=6,by=0.1)
  N_grm=length(thetas_grm)
  Pfun_grm<-function(alpha=1,theta,delta,D=1){
    z=D*alpha*(theta-delta)
    return(1/(1+exp(-z)))
  }
  ##########################################################################################
  # RATING SCALE RESPONSE
  ##########################################################################################
  thetas_rsm<-seq(from=-6,to=6,by=0.1)
  deltas_rsm<-seq(from=-6,to=6,by=0.1)
  alphas_rsm<-seq(from=-6,to=6,by=0.1)
  N_rsm=length(thetas_rsm)
  p1num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p2num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p3num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p4num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p5num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p2fun_rsm<-function(alpha,theta,delta,tau1,D){
    z1<-D*alpha*(theta-(delta+tau1))
    return(exp(0+z1))
  }
  p3fun_rsm<-function(alpha,theta,delta,tau1,tau2,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    return(exp(0+z1+z2))
  }
  p4fun_rsm<-function(alpha,theta,delta,tau1,tau2,tau3,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    z3<-D*alpha*(theta-(delta+tau3))
    return(exp(0+z1+z2+z3))
  }
  p5fun_rsm<-function(alpha,theta,delta,tau1,tau2,tau3,tau4,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    z3<-D*alpha*(theta-(delta+tau3))
    z4<-D*alpha*(theta-(delta+tau4))
    return(exp(0+z1+z2+z3+z4))
  }
  ##########################################################################################
  # NOMINAL RESPONSE
  ##########################################################################################
  thetas_nrm<-seq(from=-6,to=6,by=0.1)
  deltas_nrm<-seq(from=-6,to=6,by=0.1)
  alphas_nrm<-seq(from=-6,to=6,by=0.1)
  N_nrm=length(thetas_nrm)
  p2num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p3num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p4num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p5num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p2fun_nrm<-function(alpha1,theta,delta1,D){return(exp(D*alpha1*(theta-delta1)))}
  p3fun_nrm<-function(alpha2,theta,delta2,D){return(exp(D*alpha2*(theta-delta2)))}
  p4fun_nrm<-function(alpha3,theta,delta3,D){return(exp(D*alpha3*(theta-delta3)))}
  p5fun_nrm<-function(alpha4,theta,delta4,D){return(exp(D*alpha4*(theta-delta4)))}
  ##########################################################################################
  # DICHOTOMOUS MULTIDIMENSIONAL
  ##########################################################################################
  N_mirtd<-50
  thetas1<-seq(from=-4,to=4,length.out=N_mirtd)
  thetas2<-seq(from=-4,to=4,length.out=N_mirtd)
  pjj<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
  Pfun_mirtd<-function(theta1,theta2,c,d,alpha1,alpha2,delta1,delta2,D){
    gamma<--(alpha1*delta1+alpha2*delta2)
    alpha<-matrix(c(alpha1,alpha2),nrow=2,ncol=1)
    theta<-matrix(c(theta1,theta2),nrow=2,ncol=1)
    z<-D*(t(alpha)%*%theta+gamma)
    return(c+(d-c)*(1/(1+exp(-z))))
  }
  Pfun_mirtd2<-function(theta1,theta2,c,d,alpha1,alpha2,delta1,delta2,D){
    gamma1<--(alpha1*delta1)
    gamma2<--(alpha2*delta2)
    z1<-D*(alpha1*theta1+gamma1)
    z2<-D*(alpha2*theta2+gamma2)
    return(c+(d-c)*(1/(1+exp(-z1)))*(1/(1+exp(-z2))))
  }
  ##########################################################################################
  # THURSTONIAN
  ##########################################################################################
  pn_gamma<-seq(from=0.05,to=0.6,by=0.1)
  pn_lambda<-seq(from=0.05,to=0.6,by=0.1)
  pn_eta<-seq(from=-6,to=6,by=0.1)
  pn_psi<-seq(from=-6,to=6,by=0.1)
  icc_cfa_thurstonian<-function(eta,gamma,lambda,psi) {
    return(pnorm((-gamma+lambda*eta)/(sqrt(psi))))
  }
  icc_cfa_thurstonian_bf<-function(eta,gamma,lambda_i,lambda_k,psi_i,psi_k) {
    return(pnorm((-gamma+lambda_i*eta-lambda_k*eta)/(sqrt(psi_i+psi_k))))
  }
  icc_cfa_thurstonian_l<-function(eta,alpha,beta_i,beta_k) {
    return(pnorm(alpha+(beta_i-beta_k)*eta))
  }
  shiny::shinyApp(ui=shiny::tagList(shiny::navbarPage("IRT Models using IRTDemo",
                                                      ##########################################################################################
                                                      # RACH
                                                      ##########################################################################################
                                                      shiny::tabPanel("Rasch-4PL",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                                                      shiny::sidebarPanel(shiny::selectInput("D_rach",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                                                          shiny::sliderInput("alpha_rach",label="Discrimination (a)",min=-10,max=10,value=1,step=.1),
                                                                                          shiny::sliderInput("delta_rach",label="Difficulty (b)",min=-10,max=10,value=0,step=.1),
                                                                                          shiny::sliderInput("la_rach",label="Guessing (c)",min=0,max=1,value=0,step=.01),
                                                                                          shiny::sliderInput("ua_rach",label="Inattentiveness (d)",min=0,max=1,value=1,step=.01),
                                                                                          shiny::selectInput("modprob_rach",label="Modeled Probability",list("P(X=1|theta)"=1,"P(X|theta)"=2),selected=2),
                                                                                          shiny::textOutput(outputId="feedback")),
                                                                      shiny::mainPanel(#shiny::plotOutput(outputId="dich_plot",width="100%"),
                                                                        plotly::plotlyOutput(outputId="rach_dichotomous_plotly",width="100%",height="700px")
                                                                      )),
                                                      ##########################################################################################
                                                      # PARTIAL CREDIT
                                                      ##########################################################################################
                                                      shiny::tabPanel("PCM-GPCM",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                                                      shiny::sidebarPanel(shiny::selectInput("D_pcm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                                                          shiny::sliderInput("alpha_pcm",label="Discrimination",min=-10,max=10,value=1,step=.5),
                                                                                          shiny::sliderInput("delta1_pcm",label="Step parameter 1",min=-10,max=10,value=-2,step=.1),
                                                                                          shiny::sliderInput("delta2_pcm",label="Step parameter 2",min=-10,max=10,value=-0.75,step=.1),
                                                                                          shiny::sliderInput("delta3_pcm",label="Step parameter 3",min=-10,max=10,value=0.75,step=.1),
                                                                                          shiny::sliderInput("delta4_pcm",label="Step parameter 4",min=-10,max=10,value=2,step=.1)),
                                                                      shiny::mainPanel(#shiny::plotOutput(outputId="gpcm_plot",width="100%"),
                                                                        plotly::plotlyOutput(outputId="gpcm_plotly",width="100%",height="700px"))),
                                                      ##########################################################################################
                                                      # GRADED RESPONSE
                                                      ##########################################################################################
                                                      shiny::tabPanel("GRM",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                                                      shiny::sidebarPanel(shiny::selectInput("D_grm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                                                          shiny::sliderInput("alpha_grm",label="Discrimination",min=-10,max=10,value=1.5,step=.1),
                                                                                          shiny::sliderInput("delta1_grm",label="Difficulty 1",min=-2.5,max=-1.5,value=-2,step=.1),
                                                                                          shiny::sliderInput("delta2_grm",label="Difficulty 2",min=-1.25,max=-0.25,value=-0.75,step=.1),
                                                                                          shiny::sliderInput("delta3_grm",label="Difficulty 3",min=0,max=1.25,value=0.75,step=.1),
                                                                                          shiny::sliderInput("delta4_grm",label="Difficulty 4",min=1.5,max=2.5,value=2,step=.1)),
                                                                      shiny::mainPanel(#shiny::plotOutput(outputId="grm_plot",width="100%"),
                                                                        plotly::plotlyOutput(outputId="grm_plotly",width="100%",height="700px"))),
                                                      ##########################################################################################
                                                      # RATING SCALE
                                                      ##########################################################################################
                                                      shiny::tabPanel("RSM-GRSM",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                                                      shiny::sidebarPanel(shiny::selectInput("D_rsm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                                                          shiny::sliderInput("alpha_rsm",label="Discrimination",min=-10,max=10,value=1,step=.1),
                                                                                          shiny::sliderInput("delta_rsm",label="Difficulty",min=-10,max=10,value=0,step=.1),
                                                                                          shiny::sliderInput("tau1_rsm",label="Threshold 1",min=-10,max=10,value=-1,step=.1),
                                                                                          shiny::sliderInput("tau2_rsm",label="Threshold 2",min=-10,max=10,value=-0.4,step=.1),
                                                                                          shiny::sliderInput("tau3_rsm",label="Threshold 3",min=-10,max=10,value=0.4,step=.1),
                                                                                          shiny::sliderInput("tau4_rsm",label="Threshold 4",min=-10,max=10,value=1,step=.1)),
                                                                      shiny::mainPanel(#shiny::plotOutput(outputId="grsm_plot",width="100%"),
                                                                        plotly::plotlyOutput(outputId="grsm_plotly",width="100%",height="700px"))),
                                                      ##########################################################################################
                                                      # NOMINAL RESPONSE
                                                      ##########################################################################################
                                                      shiny::tabPanel("NRM",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                                                      shiny::sidebarPanel(shiny::selectInput("D_nrm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                                                          shiny::sliderInput("alpha1_nrm",label="Discrimination 1",min=-10,max=10,value=-4,step=.1),
                                                                                          shiny::sliderInput("alpha2_nrm",label="Discrimination 2",min=-10,max=10,value=-2,step=.1),
                                                                                          shiny::sliderInput("alpha3_nrm",label="Discrimination 3",min=-10,max=10,value=2,step=.1),
                                                                                          shiny::sliderInput("alpha4_nrm",label="Discrimination 4",min=-10,max=10,value=4,step=.1),
                                                                                          shiny::sliderInput("delta1_nrm",label="Difficulty 1",min=-10,max=10,value=-4,step=.1),
                                                                                          shiny::sliderInput("delta2_nrm",label="Difficulty 2",min=-10,max=10,value=-2,step=.1),
                                                                                          shiny::sliderInput("delta3_nrm",label="Difficulty 3",min=-10,max=10,value=2,step=.1),
                                                                                          shiny::sliderInput("delta4_nrm",label="Difficulty 4",min=-10,max=10,value=4,step=.1)),
                                                                      shiny::mainPanel(#shiny::plotOutput(outputId="nrm_plot",width="100%"),
                                                                        plotly::plotlyOutput(outputId="nrm_plotly",width="100%",height="700px"))),
                                                      ##########################################################################################
                                                      # THURSTONIAN
                                                      ##########################################################################################
                                                      shiny::tabPanel("TIRT",
                                                                      shiny::tabsetPanel(shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left[\\frac{-\\gamma_l+\\lambda_l\\eta}{\\sqrt{\\psi^2_l}}\\right]$$'),
                                                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_gamma",label=shiny::withMathJax('$$\\gamma$$'),min=-10,max=10,value=0,step=.1),
                                                                                                                             shiny::sliderInput("pn_lambda",label=shiny::withMathJax('$$\\lambda$$'),min=-10,max=10,value=.5,step=.1),
                                                                                                                             shiny::sliderInput("pn_eta",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1),
                                                                                                                             shiny::sliderInput("pn_psi",label=shiny::withMathJax('$$\\psi$$'),min=0,max=10,value=.5,step=.1)),
                                                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot",width="100%"),
                                                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly",width="100%",height="700px"))),
                                                                                         shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left[\\frac{-\\gamma+\\lambda_l\\eta_α-\\lambda_k\\eta_b}{\\sqrt{\\psi^2_i+\\psi^2_k}}\\right]$$'),
                                                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_gamma_l",label=shiny::withMathJax('$$\\gamma$$'),min=-10,max=10,value=0,step=.1),
                                                                                                                             shiny::sliderInput("pn_lambda_i",label=shiny::withMathJax('$$\\lambda_i$$'),min=-10,max=10,value=.5,step=.1),
                                                                                                                             shiny::sliderInput("pn_lambda_k",label=shiny::withMathJax('$$\\lambda_k$$'),min=-10,max=10,value=0,step=.1),
                                                                                                                             shiny::sliderInput("pn_eta_ab",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1),
                                                                                                                             shiny::sliderInput("pn_psi_i",label=shiny::withMathJax('$$\\psi_i$$'),min=0,max=10,value=.5,step=.1),
                                                                                                                             shiny::sliderInput("pn_psi_k",label=shiny::withMathJax('$$\\psi_k$$'),min=0,max=10,value=.5,step=.1)),
                                                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot1",width="100%"),
                                                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly1",width="100%",height="700px"))),
                                                                                         shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left(\\alpha+\\left(\\beta_i-\\beta_k\\right)\\eta\\right)$$'),
                                                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_alpha",label=shiny::withMathJax('$$\\alpha$$'),min=-10,max=10,value=0,step=.1),
                                                                                                                             shiny::sliderInput("pn_beta_i",label=shiny::withMathJax('$$\\beta_i$$'),min=-10,max=10,value=.5,step=.1),
                                                                                                                             shiny::sliderInput("pn_beta_k",label=shiny::withMathJax('$$\\beta_k$$'),min=-10,max=10,value=0,step=.1),
                                                                                                                             shiny::sliderInput("pn_eta_abl",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1)),
                                                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot2",width="100%"),
                                                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly2",width="100%",height="700px"))))),
                                                      ##########################################################################################
                                                      # MULTIDIMENSIONAL DICHOTOMOUS
                                                      ##########################################################################################
                                                      shiny::tabPanel("MIRT Dichotomous",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                                                      shiny::sidebarPanel(width=3,shiny::selectInput("D_mdm",label="Item Response Function",list("Normal Ogive"=1,"Logistic"=2),selected=2),
                                                                                          shiny::selectInput("comp_mdm",label="Compensatory",list("Yes"=1,"No"=2),selected=1),
                                                                                          shiny::sliderInput("delta1_mdm",label="Location 1",min=-10,max=10,value=0,step=0.1),
                                                                                          shiny::sliderInput("delta2_mdm",label="Location 2",min=-10,max=10,value=0,step=0.1),
                                                                                          shiny::sliderInput("alpha1_mdm",label="Discrimination 1",min=-10,max=10,value=1,step=0.1),
                                                                                          shiny::sliderInput("alpha2_mdm",label="Discrimination 2",min=-10,max=10,value=1,step=0.1),
                                                                                          shiny::sliderInput("c_mdm",label="Lower Asymptote",min=0,max=.4,value=0,step=0.1),
                                                                                          shiny::sliderInput("d_mdm",label="Upper Asymptote",min=0,max=1,value=1,step=0.1),
                                                                                          shiny::textInput("nametheta1_mdm",label="Name Dimension 1","Math"),
                                                                                          shiny::textInput("nametheta2_mdm",label="Name Dimension 2","Reading")),
                                                                      shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Item Response Surface",
                                                                                                                          shiny::mainPanel(plotly::plotlyOutput(outputId="plotplotly",width="100%",height="800px"))),
                                                                                                          shiny::tabPanel("Contour Plot",shiny::inputPanel(
                                                                                                            shiny::sliderInput("nlevels_mdm",label="N Contour Levels",min=10,max=100,value=10,step=1)),
                                                                                                            shiny::mainPanel(shiny::plotOutput(outputId="plotcont",width="100%",height="700px"))),
                                                                                                          shiny::tabPanel("Information Plot",
                                                                                                                          shiny::mainPanel(plotly::plotlyOutput(outputId="infoplotly",width="100%",height="800px"))))))
  )),server=function(input,output){
    ##########################################################################################
    # RACH
    ##########################################################################################
    # output$dich_plot<-renderPlot({
    #   D<-switch(input$D_rach,"1"=1,"2"=1.702)
    #   for(j in 1:N_rach){ p1_rach[j]<-Pfun_rach(D=D,la=input$la_rach,ua=input$ua_rach,theta=thetas_rach[j],delta=input$delta_rach,alpha=input$alpha_rach) }
    #   if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach==1){title<-c("Rasch Model")}
    #   if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach!=1){title<-c("1PL or 2PL Model")}
    #   if(input$ua_rach==1 & input$la_rach!=0){title<-c("3PL Model")}
    #   if(input$la_rach!=0 & input$ua_rach!=1){title<-c("4PL Model")}
    #   output$feedback<-renderText({ ifelse(input$ua_rach!=1 & input$la_rach==0,"No such model!",paste(title,": a=",input$alpha_rach," b=",input$delta_rach," c=",input$la_rach," d=",input$ua_rach)) })
    #   graphics::plot(NULL,ylab=expression(P(X)),xlab=expression(theta),main=paste(title),xlim=c(-6,6),ylim=c(0,1))
    #   modprob<-switch(input$modprob_rach,"1"=1,"2"=2)
    #   if(modprob==1){
    #     lines(thetas_rach,p1_rach,type="l",xlim=c(-6,6),col=1)
    #     legend(legend="P(X=1|theta)",col=1,lty=1,"right")
    #   }
    #   if(modprob==2){
    #     lines(thetas_rach,p1_rach,type="l",xlim=c(-6,6),col=1)
    #     lines(thetas_rach,1-p1_rach,type="l",xlim=c(-6,6),col=2)
    #     legend(legend=c("P(X=1|theta)","P(X=0|theta)"),col=c(1,2),lty=1,"right")
    #   }
    # })
    output$rach_dichotomous_plotly<-plotly::renderPlotly({
      D<-switch(input$D_rach,"1"=1,"2"=1.702)
      for(j in 1:N_rach){ p1_rach[j]<-Pfun_rach(D=D,la=input$la_rach,ua=input$ua_rach,theta=thetas_rach[j],delta=input$delta_rach,alpha=input$alpha_rach) }
      if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach==1){title<-c("Rasch Model")}
      if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach!=1){title<-c("1PL or 2PL Model")}
      if(input$ua_rach==1 & input$la_rach!=0){title<-c("3PL Model")}
      if(input$la_rach!=0 & input$ua_rach!=1){title<-c("4PL Model")}
      output$feedback<-shiny::renderText({ifelse(input$ua_rach!=1 & input$la_rach==0,"No such model!",paste(title,": a=",input$alpha_rach," b=",input$delta_rach," c=",input$la_rach," d=",input$ua_rach))})
      modprob<-switch(input$modprob_rach,"1"=1,"2"=2)
      if(modprob==1){
        df<-data.frame(x=thetas_rach,y=p1_rach,type="P(X=1|theta)")
      }
      if(modprob==2){
        df<-rbind(data.frame(x=thetas_rach,y=p1_rach,type="P(X=1|theta)"),
                  data.frame(x=thetas_rach,y=1-p1_rach,type="P(X=0|theta)"))
      }
      plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
        plotly::layout(title=list(text=title),
                       xaxis=list(title="θ"),
                       yaxis=list(title="P(x)",range=c(0,1)),
                       legend=list(x=0,y=.99))%>%
        plotly::add_segments(x=min(thetas_rach,na.rm=TRUE),xend=max(thetas_rach,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    # PARTIAL CREDIT
    ##########################################################################################
    # output$gpcm_plot<-renderPlot({
    #   D<-switch(input$D_pcm,"1" = 1,"2" = 1.702)
    #   if(input$alpha_pcm==0){stop("Why would you have an item with 0(zero) discrimination!")}
    #   p1num_pcm=1
    #   for(j in 1:N_pcm){
    #     p2num_pcm[j]<-p2fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm)
    #     p3num_pcm[j]<-p3fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm)
    #     p4num_pcm[j]<-p4fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm)
    #     p5num_pcm[j]<-p5fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm,delta4=input$delta4_pcm)
    #   }
    #   pdenom<-1+p2num_pcm+p3num_pcm+p4num_pcm+p5num_pcm
    #   p1<-p1num_pcm/pdenom
    #   p2<-p2num_pcm/pdenom
    #   p3<-p3num_pcm/pdenom
    #   p4<-p4num_pcm/pdenom
    #   p5<-p5num_pcm/pdenom
    #   title<-ifelse(input$alpha_pcm==1,"Partial Credit Model (Masters,1982)","Generalized Partial Credit Model (Muraki,1992)")
    #   graphics::plot(NULL,ylab=expression(P(X)),xlab=expression(theta),main=paste(title),xlim=c(-6,6),ylim=c(0,1))
    #   lines(thetas_pcm,p1,type="l",xlim=c(-6,6),col=2)
    #   lines(thetas_pcm,p2,type="l",xlim=c(-6,6),col=3)
    #   lines(thetas_pcm,p3,type="l",xlim=c(-6,6),col=4)
    #   lines(thetas_pcm,p4,type="l",xlim=c(-6,6),col=5)
    #   lines(thetas_pcm,p5,type="l",xlim=c(-6,6),col=6)
    #   legend(legend=c("P(X=1|theta)","P(X=2|theta)","P(X=3|theta)","P(X=4|theta)","P(X=5|theta)"),col=2:6,lty=1,"right")
    # })
    output$gpcm_plotly<-plotly::renderPlotly({
      D<-switch(input$D_pcm,"1" = 1,"2" = 1.702)
      if(input$alpha_pcm==0){stop("Why would you have an item with 0(zero) discrimination!")}
      p1num_pcm=1
      for(j in 1:N_pcm){
        p2num_pcm[j]<-p2fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm)
        p3num_pcm[j]<-p3fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm)
        p4num_pcm[j]<-p4fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm)
        p5num_pcm[j]<-p5fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm,delta4=input$delta4_pcm)
      }
      pdenom<-1+p2num_pcm+p3num_pcm+p4num_pcm+p5num_pcm
      p1<-p1num_pcm/pdenom
      p2<-p2num_pcm/pdenom
      p3<-p3num_pcm/pdenom
      p4<-p4num_pcm/pdenom
      p5<-p5num_pcm/pdenom
      title<-ifelse(input$alpha_pcm==1,"Partial Credit Model (Masters,1982)","Generalized Partial Credit Model (Muraki,1992)")
      df<-rbind(data.frame(x=thetas_pcm,y=p1,type="P(X=1|theta)"),
                data.frame(x=thetas_pcm,y=p2,type="P(X=2|theta)"),
                data.frame(x=thetas_pcm,y=p3,type="P(X=3|theta)"),
                data.frame(x=thetas_pcm,y=p4,type="P(X=4|theta)"),
                data.frame(x=thetas_pcm,y=p5,type="P(X=5|theta)"))
      plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
        plotly::layout(title=title,
               xaxis=list(title="θ"),
               yaxis=list(title="P(x)",range=c(0,1)),
               legend=list(x=0,y=.99))%>%
        plotly::add_segments(x=min(thetas_pcm,na.rm=TRUE),xend=max(thetas_pcm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    # GRADED RESPONSE
    ##########################################################################################
    # output$grm_plot<-renderPlot({
    #   D<-switch(input$D_grm,"1"=1,"2"=1.702)
    #   p<-matrix(NA,nrow=N_grm,4)
    #   for(j in 1:N_grm){
    #     p[j,1]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta1_grm,alpha=input$alpha_grm)
    #     p[j,2]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta2_grm,alpha=input$alpha_grm)
    #     p[j,3]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta3_grm,alpha=input$alpha_grm)
    #     p[j,4]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta4_grm,alpha=input$alpha_grm)
    #   }
    #   graphics::plot(NULL,ylab="P(X=m|theta)",xlab=expression(theta),main="Graded Response Model",xlim=c(-6,6),ylim=c(0,1))
    #   lines(thetas_grm,1-p[,1],type="l",xlim=c(-6,6),col=2)
    #   lines(thetas_grm,p[,1]-p[,2],type="l",xlim=c(-6,6),col=3)
    #   lines(thetas_grm,p[,2]-p[,3],type="l",xlim=c(-6,6),col=4)
    #   lines(thetas_grm,p[,3]-p[,4],type="l",xlim=c(-6,6),col=5)
    #   lines(thetas_grm,p[,4]-0,type="l",xlim=c(-6,6),col=6)
    #   legend(legend=c("P(X=1|theta)","P(X=2|theta)","P(X=3|theta)","P(X=4|theta)","P(X=5|theta)"),col=2:6,lty=1,"right")
    # })
    output$grm_plotly<-plotly::renderPlotly({
      D<-switch(input$D_grm,"1"=1,"2"=1.702)
      p<-matrix(NA,nrow=N_grm,4)
      for(j in 1:N_grm){
        p[j,1]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta1_grm,alpha=input$alpha_grm)
        p[j,2]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta2_grm,alpha=input$alpha_grm)
        p[j,3]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta3_grm,alpha=input$alpha_grm)
        p[j,4]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta4_grm,alpha=input$alpha_grm)
      }
      title<-"Graded Response Model"
      df<-rbind(data.frame(x=thetas_grm,y=1-p[,1],type="P(X=1|theta)"),
                data.frame(x=thetas_grm,y=p[,1]-p[,2],type="P(X=2|theta)"),
                data.frame(x=thetas_grm,y=p[,2]-p[,3],type="P(X=3|theta)"),
                data.frame(x=thetas_grm,y=p[,3]-p[,4],type="P(X=4|theta)"),
                data.frame(x=thetas_grm,y=p[,4]-0,type="P(X=5|theta)"))
      plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
        plotly::layout(title=title,
               xaxis=list(title="θ"),
               yaxis=list(title="P(X=m|theta)",range=c(0,1)),
               legend=list(x=0,y=.99))%>%
        plotly::add_segments(x=min(thetas_grm,na.rm=TRUE),xend=max(thetas_grm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    # RATING SCALE
    ##########################################################################################
    # output$grsm_plot<-renderPlot({
    #   D<-switch(input$D_rsm,"1"=1,"2"=1.702)
    #   if(input$alpha_rsm==0){stop("Why would you have an item with 0 (zero) discrimination!")}
    #   p1num_rsm=1
    #   for(j in 1:N_rsm){
    #     p2num_rsm[j]<-p2fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm)
    #     p3num_rsm[j]<-p3fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm)
    #     p4num_rsm[j]<-p4fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm)
    #     p5num_rsm[j]<-p5fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm,tau4=input$tau4_rsm)
    #   }
    #   pdenom<-1+p2num_rsm+p3num_rsm+p4num_rsm+p5num_rsm
    #   p1<-p1num_rsm/pdenom
    #   p2<-p2num_rsm/pdenom
    #   p3<-p3num_rsm/pdenom
    #   p4<-p4num_rsm/pdenom
    #   p5<-p5num_rsm/pdenom
    #   title<-ifelse(input$alpha_rsm==1,"Rating Scale Model (Andrich,1978)","Generalized Rating Scale Model (Muraki,1990)")
    #   graphics::plot(NULL,ylab="P(X=m|theta)",xlab=expression(theta),main=paste(title),xlim=c(-6,6),ylim=c(0,1))
    #   lines(thetas_rsm,p1,type="l",xlim=c(-6,6),col=2)
    #   lines(thetas_rsm,p2,type="l",xlim=c(-6,6),col=3)
    #   lines(thetas_rsm,p3,type="l",xlim=c(-6,6),col=4)
    #   lines(thetas_rsm,p4,type="l",xlim=c(-6,6),col=5)
    #   lines(thetas_rsm,p5,type="l",xlim=c(-6,6),col=6)
    #   legend(legend=c("P(X=1|theta)","P(X=2|theta)","P(X=3|theta)","P(X=4|theta)","P(X=5|theta)"),col=2:6,lty=1,"right")
    # })
    output$grsm_plotly<-plotly::renderPlotly({
      D<-switch(input$D_rsm,"1"=1,"2"=1.702)
      if(input$alpha_rsm==0){stop("Why would you have an item with 0 (zero) discrimination!")}
      p1num_rsm=1
      for(j in 1:N_rsm){
        p2num_rsm[j]<-p2fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm)
        p3num_rsm[j]<-p3fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm)
        p4num_rsm[j]<-p4fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm)
        p5num_rsm[j]<-p5fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm,tau4=input$tau4_rsm)
      }
      pdenom<-1+p2num_rsm+p3num_rsm+p4num_rsm+p5num_rsm
      p1<-p1num_rsm/pdenom
      p2<-p2num_rsm/pdenom
      p3<-p3num_rsm/pdenom
      p4<-p4num_rsm/pdenom
      p5<-p5num_rsm/pdenom
      title<-ifelse(input$alpha_rsm==1,"Rating Scale Model (Andrich,1978)","Generalized Rating Scale Model (Muraki,1990)")
      df<-rbind(data.frame(x=thetas_rsm,y=p1,type="P(X=1|theta)"),
                data.frame(x=thetas_rsm,y=p2,type="P(X=2|theta)"),
                data.frame(x=thetas_rsm,y=p3,type="P(X=3|theta)"),
                data.frame(x=thetas_rsm,y=p4,type="P(X=4|theta)"),
                data.frame(x=thetas_rsm,y=p5,type="P(X=5|theta)"))
      plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
        plotly::layout(title=title,
               xaxis=list(title="θ"),
               yaxis=list(title="P(X=m|theta)",range=c(0,1)),
               legend=list(x=0,y=.99))%>%
        plotly::add_segments(x=min(thetas_rsm,na.rm=TRUE),xend=max(thetas_rsm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    # NOMINAL RESPONSE
    ##########################################################################################
    # output$nrm_plot<-renderPlot({
    #   D<-switch(input$D_nrm,"1"=1,"2"=1.702)
    #   p1num_nrm=1
    #   for(j in 1:N_nrm){
    #     p2num_nrm[j]<-p2fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha1_nrm,delta1=input$delta1_nrm)
    #     p3num_nrm[j]<-p3fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha2_nrm,delta2=input$delta2_nrm)
    #     p4num_nrm[j]<-p4fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha3_nrm,delta3=input$delta3_nrm)
    #     p5num_nrm[j]<-p5fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha4_nrm,delta4=input$delta4_nrm)
    #   }
    #   pdenom<-1+p2num_nrm+p3num_nrm+p4num_nrm+p5num_nrm
    #   p1<-p1num_nrm/pdenom
    #   p2<-p2num_nrm/pdenom
    #   p3<-p3num_nrm/pdenom
    #   p4<-p4num_nrm/pdenom
    #   p5<-p5num_nrm/pdenom
    #   graphics::plot(NULL,ylab="P(X=m|theta)",xlab=expression(theta),main="Nominal Response Model",sub="Note: Baker's (1992) reparamterization used.",xlim=c(-6,6),ylim=c(0,1))
    #   lines(thetas_nrm,p1,type="l",xlim=c(-6,6),col=2)
    #   lines(thetas_nrm,p2,type="l",xlim=c(-6,6),col=3)
    #   lines(thetas_nrm,p3,type="l",xlim=c(-6,6),col=4)
    #   lines(thetas_nrm,p4,type="l",xlim=c(-6,6),col=5)
    #   lines(thetas_nrm,p5,type="l",xlim=c(-6,6),col=6)
    #   legend(legend=c("P(X=a|theta)","P(X=b|theta)","P(X=c|theta)","P(X=d|theta)","P(X=e|theta)"),col=2:6,lty=1,"right")
    # })
    output$nrm_plotly<-plotly::renderPlotly({
      D<-switch(input$D_nrm,"1"=1,"2"=1.702)
      p1num_nrm=1
      for(j in 1:N_nrm){
        p2num_nrm[j]<-p2fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha1_nrm,delta1=input$delta1_nrm)
        p3num_nrm[j]<-p3fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha2_nrm,delta2=input$delta2_nrm)
        p4num_nrm[j]<-p4fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha3_nrm,delta3=input$delta3_nrm)
        p5num_nrm[j]<-p5fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha4_nrm,delta4=input$delta4_nrm)
      }
      pdenom<-1+p2num_nrm+p3num_nrm+p4num_nrm+p5num_nrm
      p1<-p1num_nrm/pdenom
      p2<-p2num_nrm/pdenom
      p3<-p3num_nrm/pdenom
      p4<-p4num_nrm/pdenom
      p5<-p5num_nrm/pdenom
      title<-"Nominal Response Model"
      subtitle<-"Note: Baker's (1992) reparamterization used."
      df<-rbind(data.frame(x=thetas_nrm,y=p1,type="P(X=a|theta)"),
                data.frame(x=thetas_nrm,y=p2,type="P(X=b|theta)"),
                data.frame(x=thetas_nrm,y=p3,type="P(X=c|theta)"),
                data.frame(x=thetas_nrm,y=p4,type="P(X=d|theta)"),
                data.frame(x=thetas_nrm,y=p5,type="P(X=e|theta)"))
      plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
        plotly::layout(title=title,subtitle=subtitle,
               xaxis=list(title="θ"),
               yaxis=list(title="P(X=m|theta)",range=c(0,1)),
               legend=list(x=0,y=.99))%>%
        plotly::add_segments(x=min(thetas_nrm,na.rm=TRUE),xend=max(thetas_nrm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    # MULTIDIMENSIONAL DICHOTOMOUS
    ##########################################################################################
    pjj<-reactive({
      if(input$alpha1_mdm==0 | input$alpha2_mdm==0){stop("Select a discrimination value other than 0 (zero)!")}
      D<-switch(input$D_mdm,"1"=1,"2"=1.702)
      comp<-switch(input$comp_mdm,"1"=1,"2"=2)
      pjj<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
      if(comp==1){
        for(j1 in 1:N_mirtd){
          for (j2 in 1:N_mirtd){ pjj[j1,j2]<-Pfun_mirtd(theta1=thetas1[j1],theta2=thetas2[j2],c=input$c_mdm,d=input$d_mdm,alpha1=input$alpha1_mdm,alpha2=input$alpha2_mdm,delta1=input$delta1_mdm,delta2=input$delta2_mdm,D=D) }
        }
      }
      if(comp==2){
        for(j1 in 1:N_mirtd){
          for (j2 in 1:N_mirtd){ pjj[j1,j2]<-Pfun_mirtd2(theta1=thetas1[j1],theta2=thetas2[j2],c=input$c_mdm,d=input$d_mdm,alpha1=input$alpha1_mdm,alpha2=input$alpha2_mdm,delta1=input$delta1_mdm,delta2=input$delta2_mdm,D=D) }
        }
      }
      pjj
    })
    output$plotplotly<-plotly::renderPlotly({
      pjj<-pjj()
      p3D<-plotly::plot_ly(x=thetas1,y=thetas2,z=pjj,width=700,height=700,showscale=FALSE) %>%
        plotly::layout(scene=list(xaxis=list(title=input$nametheta1_mdm),yaxis=list(title=input$nametheta2_mdm),zaxis=list(title="P(Y=1)",range=c(0,1)),camera=list(eye=list(x=1.5,y=-1.5,z=1.5)))) %>%
        plotly::add_surface(opacity=1)
    })
    output$plotcont<-shiny::renderPlot({
      pjj<-pjj()
      c<-input$c_mdm
      d<-input$d_mdm
      alpha1<-input$alpha1_mdm
      alpha2<-input$alpha2_mdm
      delta1<-input$delta1_mdm
      delta2<-input$delta2_mdm
      gamma<--(alpha1*delta1+alpha2*delta2)
      omega1<-acos(alpha1/sqrt(alpha1^2+alpha2^2)) #in radians
      omega2<-acos(alpha2/sqrt(alpha1^2+alpha2^2)) #in radians
      Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
      Di<--gamma/Ai
      graphics::contour(thetas1,thetas2,pjj,nlevels=input$nlevels_mdm,xlab=input$nametheta1_mdm,ylab=input$nametheta2_mdm)
      abline(h=0,v=0)
      if(c==0 & d==1){
        cont<-contourLines(thetas1,thetas2,pjj,levels=0.5)
        lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
      }
      if(c>0 & d==1){
        cont<-contourLines(thetas1,thetas2,pjj,levels=(c+1)/2)
        lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
      }
      if(c>0 & d<1){
        cont<-contourLines(thetas1,thetas2,pjj,levels=(c+d)/2)
        lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
      }
      comp<-switch(input$comp_mdm,"1"=1,"2"=2)
      if(comp==1){
        if(alpha2>0){ arrows(Di*cos(omega1),Di*sin(omega1),x1=(Di+Ai)*cos(omega1),y1=(Di+Ai)*sin(omega1),code=2,length=0.1,col="blue") }
        if(alpha2<0){ arrows((Di+Ai)*cos(omega1),-(Di+Ai)*sin(omega1),x1=Di*cos(omega1),y1=-Di*sin(omega1),code=1,length=0.1,col="blue") }
      }
    })
    output$infopersp<-renderPlot({
      c<-input$c_mdm
      d<-input$d_mdm
      alpha1<-input$alpha1_mdm
      alpha2<-input$alpha2_mdm
      Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
      pjj<-pjj()
      info<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
      for(j1 in 1:N_mirtd){
        for (j2 in 1:N_mirtd){ info[j1,j2]<-(Ai^2)*((pjj[j1,j2]-c)^2)*((d-pjj[j1,j2])^2)/((d-c)^2)*((1-pjj[j1,j2])/pjj[j1,j2]) # (Magis,2013)
        }
      }
      graphics::persp(thetas1,thetas2,info,theta=input$angleinfo1_mdm,phi=input$angleinfo2_mdm,zlim=c(0,max(info)),xlab=input$nametheta1_mdm,ylab=input$nametheta2_mdm,zlab="Information",nticks=5,ticktype="detailed")
    })
    output$infoplotly<-plotly::renderPlotly({
      c<-input$c_mdm
      d<-input$d_mdm
      alpha1<-input$alpha1_mdm
      alpha2<-input$alpha2_mdm
      Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
      pjj<-pjj()
      info<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
      for(j1 in 1:N_mirtd){
        for (j2 in 1:N_mirtd){ info[j1,j2]<-(Ai^2)*((pjj[j1,j2]-c)^2)*((d-pjj[j1,j2])^2)/((d-c)^2)*((1-pjj[j1,j2])/pjj[j1,j2]) # (Magis,2013)
        }
      }
      plotly::plot_ly(x=thetas1,y=thetas2,z=info,width=700,height=700,showscale=FALSE) %>%
        plotly::layout(scene=list(xaxis=list(title=input$nametheta1_mdm),yaxis=list(title=input$nametheta2_mdm),zaxis=list(title="Information"),camera=list(eye=list(x=-1.5,y=1.5,z=1.5)))) %>%
        plotly::add_surface(opacity=1)
    })
    ##########################################################################################
    # THURSTONIAN
    ##########################################################################################
    # output$dichotomous_plot<-renderPlot({
    #   eta<-seq(-input$pn_eta,input$pn_eta,0.001)
    #   result<-icc_cfa_thurstonian(eta=eta,gamma=input$pn_gamma,lambda=input$pn_lambda,psi=input$pn_psi)
    #   plot(eta,result,ylab=expression(P(eta)),xlab=expression(eta),main="",ylim=c(0,1))
    # })
    # output$dichotomous_plot1<-renderPlot({
    #   eta<-seq(-input$pn_eta_ab,input$pn_eta_ab,0.001)
    #   result<-icc_cfa_thurstonian_bf(eta=eta,gamma=input$pn_gamma_l,lambda_i=input$pn_lambda_i,lambda_k=input$pn_lambda_k,psi_i=input$pn_psi_i,psi_k=input$pn_psi_k)
    #   plot(eta,result,ylab=expression(P(eta)),xlab=expression(eta),main="",ylim=c(0,1))
    # })
    # output$dichotomous_plot2<-renderPlot({
    #   eta<-seq(-input$pn_eta_abl,input$pn_eta_abl,0.001)
    #   result<-icc_cfa_thurstonian_l(eta=eta,alpha=input$pn_alpha,beta_i=input$pn_beta_i,beta_k=input$pn_beta_k)
    #   plot(eta,result,ylab=expression(P(eta)),xlab=expression(eta),main="",ylim=c(0,1))
    # })
    output$dichotomous_plotly<-plotly::renderPlotly({
      eta<-seq(-input$pn_eta,input$pn_eta,0.1)
      result<-icc_cfa_thurstonian(eta=eta,gamma=input$pn_gamma,lambda=input$pn_lambda,psi=input$pn_psi)
      df<-data.frame(eta,result)
      plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
        plotly::layout(title="",
               xaxis=list(title="η"),
               yaxis=list(title="P(η)",range=c(0,1)))%>%
        plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    output$dichotomous_plotly1<-plotly::renderPlotly({
      eta<-seq(-input$pn_eta_ab,input$pn_eta_ab,0.1)
      result<-icc_cfa_thurstonian_bf(eta=eta,gamma=input$pn_gamma_l,lambda_i=input$pn_lambda_i,lambda_k=input$pn_lambda_k,psi_i=input$pn_psi_i,psi_k=input$pn_psi_k)
      df<-data.frame(eta,result)
      plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
        plotly::layout(title="",
               xaxis=list(title="η"),
               yaxis=list(title="P(η)",range=c(0,1)))%>%
        plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    output$dichotomous_plotly2<-plotly::renderPlotly({
      eta<-seq(-input$pn_eta_abl,input$pn_eta_abl,0.1)
      result<-icc_cfa_thurstonian_l(eta=eta,alpha=input$pn_alpha,beta_i=input$pn_beta_i,beta_k=input$pn_beta_k)
      df<-data.frame(eta,result)
      plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
        plotly::layout(title="",
               xaxis=list(title="η"),
               yaxis=list(title="P(η)",range=c(0,1)))%>%
        plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
    })
    ##########################################################################################
    #
    ##########################################################################################
  }
  )
}

