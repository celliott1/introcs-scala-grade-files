import scala.util._
import scala.io._
import scala.math._
import edu.luc.cs.ui._
import scala.util.{ Try, Success, Failure }
import java.io._

object gradefiles extends App {

  val coursename = promptLine("Enter Coursename: ")
  val filegrade = GradeCalc(coursename)
    
    def GradeCalc(coursename: String) {
        val textfile = new PrintWriter(new File(coursename + "_solution.txt"))
        val categoriesarray = Source.fromFile("categories_"+coursename+".txt").getLines.toArray
        val catlength = categoriesarray.length
        val arraysize = (catlength/3)
        val headerarray = categoriesarray.slice(0,(arraysize))
        val weightarray = categoriesarray.slice(arraysize, (arraysize*2))
        val gradearray = categoriesarray.slice((arraysize*2), (arraysize*3))
        var job = gradearray.mkString
        var candle = job.split(", ")
        var dang = 0
        for(i<-0 until candle.length){
            dang = dang + (candle(i).toInt)
        }
        val numberofgrades = dang
        var kin = weightarray.mkString
        var lop = kin.split(", ")
        var hit = 0
        for(i<-0 until lop.length){
            hit = hit + (lop(i).toInt)
        }
        val weightsum = hit
        
        val studentsarray = Source.fromFile("students_"+coursename+".txt").getLines.toArray
        val studentsize = (studentsarray.length/3)
        var testarray = 0
        var bob = studentsarray
        while(bob.length!=0){
            
            var kids = bob.slice(0,3)
            var testarray = kids
            var jim = testarray.slice(0,1)
            var whoa = jim.mkString
            var dang = whoa.split(", ")
            var id = dang.slice(0,1)
            var last = dang.slice(1,2)
            var first = dang.slice(2,3)
      
            val studentdata = Source.fromFile(id.mkString+coursename+".data").getLines.toArray
            var exams = 0
            var labs = 0
            var homework = 0
            var project = 0
            var classparticipation = 0
            var goat = studentdata
               
             
               var lip = headerarray.mkString
               var pin = lip.split(", ")
               var catgrade = 0.0
               var dalist = ""
            while(pin.length!= 0){
               for(i<-0 until studentdata.length){
                   if(studentdata(i).contains(pin(0))){
                       var secondsec = studentdata(i).mkString
                       var thirdsec = secondsec.split(", ")
                       catgrade = catgrade + (thirdsec(2).toDouble)
                   }
               }
               dalist = dalist + s"$catgrade, "    
               pin = pin.drop(1)
               catgrade = 0.0
            }
                var golem = dalist.split(", ")
                var cheese = weightarray.mkString
                var hop = cheese.split(", ")
                var apple = headerarray.mkString
                var dom = apple.split(", ")
                var hot = gradearray.mkString
                var cot = hot.split(", ")
                var gradeval = 0.0
                var gradevalF = 0.0
                var gradecalc = 0.0
                
            while(dom.length!=0){
                for(i<-0 until studentdata.length){
                    if(studentdata(i).contains(dom(0))){
                         gradeval = golem(0).toDouble/(cot(0).toDouble) //category name here
                         gradevalF = gradeval * (hop(0).toDouble)
                    }  
                }
                golem = golem.drop(1)
                dom = dom.drop(1)
                cot = cot.drop(1)
                hop = hop.drop(1)
                gradecalc = gradecalc + gradevalF
            }
                

            var gradetotal = ((gradecalc)/(weightsum))
            var gradetotalz = BigDecimal(gradetotal).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
            var letter = "A"
            
            if(gradetotalz >= 93.0){
                letter = "A"
            }else if(gradetotalz >= 90.0) {
                letter = "A-"
            }else if(gradetotalz >= 87.0) {
                letter = "B+"
            }else if(gradetotalz >= 83.0) {
                letter = "B"
            }else if(gradetotalz >= 79.0) {
                letter = "B-"
            }else if(gradetotalz >= 77.0) {
                letter = "C+"
            }else if(gradetotalz >= 73.0) {
                letter = "C"
            }else if(gradetotalz >= 69.0) {
                letter = "C-"
            }else if(gradetotalz >= 67.0) {
                letter = "D+"
            }else if(gradetotalz >= 60.0) {
                letter = "D"
            }else if(gradetotalz <=60.0) {
                letter = "F"
            }
                var dart = headerarray.mkString
                var orange = dart.split(", ")
                var lemon = gradearray.mkString
                var tree = lemon.split(", ")
                var extra = ""
                var lot = 0
                var testcat = ""
                var numb = 0
                var bud = studentdata
                
            
            //if statement if student is missing a gradetotal
            if(numberofgrades != studentdata.length){
                for(i<-0 until orange.length){
                testcat = orange(i).mkString
                lot = tree(0).toInt
                var lol = testcat.toArray
                var albert = lol(0)       
                numb = 0
                bud = studentdata
                for(i<-0 until bud.length){
                    if(bud(i).contains(testcat)){
                        numb = numb + 1
                    }
                }  
                if(lot != numb){
                    var missing = lot - numb
                    extra = extra + " " + missing + albert
                }
                tree = tree.drop(1)
                }
                   textfile.write(last.mkString+", "+first.mkString+": "+gradetotalz+ " "+ letter + " Missing: "+ extra+ "\n")
            }else{
            textfile.write(last.mkString+", "+first.mkString+": "+gradetotalz+ " "+ letter+"\n")
                }
            bob = bob.drop(1)    
        }
        textfile.close()
        } 
}    

                                              
    
    