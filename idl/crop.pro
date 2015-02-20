pro crop,dopp504,dopp362,dopp906

set_plot,'ps'

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/dopp504_crop.eps

tvim,dopp504(*,*,0),title='dopp504 crop'
plots,[0,982],[962,962]
plots,[964,964],[0,998]

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/dopp362_crop.eps

tvim,dopp362(*,*,0),title='dopp362 crop'
plots,[0,982],[962,962]
plots,[964,964],[0,998]

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/dopp906_crop.eps

tvim,dopp906(*,*,0),title='dopp906 crop'
plots,[0,982],[962,962]
plots,[964,964],[0,998]

device,/close

set_plot,'x'

end