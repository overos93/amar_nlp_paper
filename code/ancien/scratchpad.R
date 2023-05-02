Protest: [protest*, petition*, rall*, disrupt* rights, justice, organizers, demonstrators]
General_News: [email, tweet, share*, copyright]

t <- modPredict2[!idxna,]
tt <- articleAnnotation[!idxna,]

ttt <- tt[tt$Protest == "0" & t$class_fixed == 1,]

ttt <- tt[tt$Protest == "1" & t$class_fixed == 0,]
