package urbanevolution

import java.io.{BufferedReader, File, FileReader}
import com.github.tototoshi.csv._

import scala.collection.mutable.ArrayBuffer

object CSV {


  /**
    * read a numerical matrix
    * @param file
    * @param sep
    * @return
    */
  def readMat(file: String,sep: String=",",naformat: String = "NA"): Array[Array[Double]] = {
    val r = new BufferedReader(new FileReader(new File(file)))
    val res = new ArrayBuffer[Array[Double]]
    var currentline = r.readLine()
    while(currentline!=null){
      res.append(currentline.split(sep).map{s => if(s.equals("NA")) Double.NaN else s.toDouble})
      currentline = r.readLine()
    }
    res.toArray
  }

  /**
    * read csv (file name)
    * @param filename
    * @param sep
    * @param withHeader
    * @return
    */
  def readCSV(filename: String, sep: String = ",", withHeader: Boolean =true): Map[String,Seq[String]] = readCSVFile(new File(filename),sep, withHeader)


  /**
    * read csv file
    * @param file
    * @param sep
    * @return
    */
  def readCSVFile(file: File,sep: String=",", withHeader: Boolean =true): Map[String,Seq[String]] = {

    implicit val readerFormat = new DefaultCSVFormat {
      override val delimiter = sep.charAt(0)
      override val quoteChar: Char = '"'
    }
    val (header,content): (Seq[String], Seq[Map[String, String]]) = if (withHeader) CSVReader.open(file)(readerFormat).allWithOrderedHeaders()
    else {
      val raw = CSVReader.open(file)(readerFormat).all()
      val h =(0 until raw(0).length).map("V"+_)
      (h,raw.map{row => row.zip(h).toMap})
    }
    header.map{case s => (s,content.map{_(s)})}.toMap
  }

}
