 use "/Users/morrisreeves/Desktop/Submission/Documentation/Do-files/final.dta"
 
 //Use 2014 and 2009 data on the prop. of two or more person households with over 100M yen in total assets as proxies for 2012 and 2007
 gen lagprophhover100M = prophhover100M[_n+2]
 
 //Use 2014 data on the 3rd quartile of net savings (savings - liabilities) for all households as a proxy for that of 2012
 gen lagsminusl_3 = sminusl_3[_n+2] 
 
 //Use 2014 and 2009 average savings of two or more person hh with at least one 65 year old member not working as proxies for 2012 and 2007
 gen lagsavings = savings[_n+2]
 
 //Encode regions of Japan (e.g. Chubu, Hokkaido)
 encode Region, gen(nRegion)
 
 //Old self-employed, without regional fixed effects; first regression table presented in text
 reg aper10000 oldSelf10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, replace ctitle(1)
 reg aper10000 oldSelf10000 elderlyper10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(2)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(3)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp above50Myen10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(4)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp above50Myen10000 RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(5)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagsavings RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(6a)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagprophhover100M RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(6b)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagsminusl_3 RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1.doc, append ctitle(6c)
 
 //Old self-employed, with regional fixed effects
 reg aper10000 oldSelf10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, replace ctitle(1)
 reg aper10000 oldSelf10000 elderlyper10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(2)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(3)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp above50Myen10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(4)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp above50Myen10000 RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(5)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagsavings RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(6a)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagprophhover100M RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(6b)
 reg aper10000 oldSelf10000 elderlyper10000 primaryProp lagsminusl_3 RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg1b.doc, append ctitle(6c)
 
 //Old farmers, without regional fixed effects; second regression table presented in text
 reg aper10000 oldFFF i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, replace ctitle(1)
 reg aper10000 oldFFF elderlyper10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(2)
 reg aper10000 oldFFF elderlyper10000 above50Myen10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(3)
 reg aper10000 oldFFF elderlyper10000 above50Myen10000 RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(4a)
 reg aper10000 oldFFF elderlyper10000 lagsavings RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(5a)
 reg aper10000 oldFFF elderlyper10000 lagprophhover100M RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(5b)
 reg aper10000 oldFFF elderlyper10000 lagsminusl_3 RojinOccupants10000 i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2.doc, append ctitle(5c)

//Old farmers, with regional fixed effects
 reg aper10000 oldFFF i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, replace ctitle(1)
 reg aper10000 oldFFF elderlyper10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(2)
 reg aper10000 oldFFF elderlyper10000 above50Myen10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(3)
 reg aper10000 oldFFF elderlyper10000 above50Myen10000 RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(4a)
 reg aper10000 oldFFF elderlyper10000 lagsavings RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(5a)
 reg aper10000 oldFFF elderlyper10000 lagprophhover100M RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(5b)
 reg aper10000 oldFFF elderlyper10000 lagsminusl_3 RojinOccupants10000 i.nRegion i.year, cluster(Prefecture) robust
 outreg2 using FinalReg2b.doc, append ctitle(5c)
 
 //Small businesses, large businesses, and the elderly per household: third regression table presented in text
 reg aperhh people65upperhh avgannualincperhh empl5to9perhh empl300plusperhh, cluster(Prefecture) robust
 outreg2 using FinalReg3.doc, replace ctitle(1)
 
