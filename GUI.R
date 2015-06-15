# Loading required packages 
require("gWidgets2") 
require("gWidgets2tcltk") 

setwd("images/")
main <- gwindow(title="Main", width=300, height=460) 

gw0 <- ggroup(horizontal=FALSE,spacing=20,container=main)

upload_group <- ggroup(horizontal=TRUE,spacing=5,container=gw0)

lbl_image <- glabel(container=upload_group, text="Image Address:") 
txt_image <- gedit(container=upload_group) 
b<-c()

import_image_btn <- gbutton(container=upload_group,expand=FALSE,text="Upload Image", handler=function(h, ...) 
  {
  image_name<-svalue(txt_image)
  b<<-readJPEG(image_name) 
  image<-b
  svalue(origImage)<-image_name  
} ) 

orig_image_frame <- gframe("Original Image",expand=FALSE,horizontal=FALSE,container=gw0,spacing=5)
size(orig_image_frame)<-c(210,225)
origImage <- gimage(filename="", dirname="",  size="", container=orig_image_frame)

fix_image_btn <- gbutton(container=gw0,expand=FALSE,text="Fix Image", handler=function(h, ...) {
  
  pixels<-which(b>=0.7 | b<=0.3 && b!=0)
  l<-length(pixels)
  inn<-sqrt(length(b)/3)
  for (i in pixels)
  { 
    if(i>inn && i<(length(b)-inn))
    {
      middlerow<-b[i-3]:b[i+3]
      toprow<-b[i+inn-3]:b[i+inn+3]
      bottomrow<-b[i-inn-3]:b[i-inn+3]
      b[i]<-mean(c(toprow,middlerow,bottomrow))
    }
  }
  
  writeJPEG(b,"good.jpg")
  
  svalue(editedImage)<-("good.jpg")
  
  
} ) 


edited_image_frame <- gframe("Edited Image",horizontal=FALSE,container=gw0,spacing=5)
size(edited_image_frame)<-c(210,225)
editedImage <- gimage(filename="", dirname="",  size="", container=edited_image_frame)
