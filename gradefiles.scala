import scala.util._
import scala.io._
import scala.math._
import edu.luc.cs.ui._
import scala.util.{ Try, Success, Failure }

object gradefiles extends App {

  val coursename = promptLine("Enter Coursename: ")
  val filegrade = GradeCalc(coursename)
    
    def GradeCalc(coursename: String) {
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
           
            while(goat.length!= 0){
               var firstsec = goat.slice(0,1)
               var secondsec = firstsec.mkString
               var thirdsec = secondsec.split(", ")
                
                 if (secondsec.contains("Ho")){
                    homework = homework + (thirdsec(2).toInt)
                }else if(secondsec.contains("La")){
                    labs = labs + (thirdsec(2).toInt)
                }else if(secondsec.contains("Ex")){
                    exams = exams + (thirdsec(2).toInt)
                }else if(secondsec.contains("Pr")){
                    project = project + (thirdsec(2).toInt)
                }else if(secondsec.contains("Cl")){
                    classparticipation = classparticipation + (thirdsec(2).toInt)
                }
                goat = goat.drop(1) 
            }
                var cheese = weightarray.mkString
                var hop = cheese.split(", ")
                var apple = headerarray.mkString
                var dom = apple.split(", ")
                var hot = gradearray.mkString
                var cot = hot.split(", ")
                var homeworkt = 0.0
                var homeworkF = 0.0
                var labst = 0.0
                var labsF = 0.0
                var examst = 0.0
                var examsF = 0.0
                var projectt = 0.0
                var projectF = 0.0
                var classparticipationt = 0.0
                var classparticipationF = 0.0
                for(i<-0 until hop.length){
                    if(dom(0).contains("Ho")){
                         homeworkt = homework.toDouble/(cot(0).toDouble)
                         homeworkF = homeworkt * (hop(0).toDouble)
                    }else if(dom(0).contains("La")){
                         labst = labs.toDouble/(cot(0).toDouble)
                         labsF = labst * (hop(0).toDouble)
                    }else if(dom(0).contains("Ex")){
                         examst = exams.toDouble/(cot(0).toDouble)
                         examsF = examst * (hop(0).toDouble)
                    }else if(dom(0).contains("Pr")){
                         projectt = project.toDouble/(cot(0).toDouble)
                         projectF = projectt * (hop(0).toDouble)
                    }else if(dom(0).contains("Cl")){
                         classparticipationt = classparticipation.toDouble/(cot(0).toDouble)
                         classparticipationF = classparticipationt * (hop(0).toDouble)
                    }
                    hop = hop.drop(1)
                    dom = dom.drop(1)
                    cot = cot.drop(1)
                }

            var gradetotal = ((homeworkF + labsF + examsF + projectF + classparticipationF)/(weightsum))
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
                   println(last.mkString+", "+first.mkString+": "+gradetotalz+ " "+ letter + " Missing: "+ extra)
            }else{
            println(last.mkString+", "+first.mkString+": "+gradetotalz+ " "+ letter)
                }
            bob = bob.drop(1)    
        }
        } 
}    

                                              
    
    