package titanic

import scala.util.{Try, Success, Failure}

object TitanicDataSet {

  /**
   * Creates a model that predicts 1 (survived) if the person of the certain record
   * is female and 0 (deceased) otherwise
   *
   * @return The model represented as a function
   */
  def simpleModel: (Map[String, Any], String) => (Any, Any) = {
    (map, idKey) => (map(idKey), if (map("sex") == "female") 1 else 0)
  }

  /**
   * This function should count for a given attribute list, how often an attribute is
   * not present in the data records of the data set
   *
   * @param data    The DataSet where the counting takes place
   * @param attList List of attributes where the missings should be counted
   * @return A Map that contains the attribute names (key) and the number of missings (value)
   */
  def countAllMissingValues(data: List[Map[String, Any]], attList: List[String]): Map[String, Int] = {
    val dataWithLowercaseAttribs = data.map(record => record.map { case (key, value) => key.toLowerCase -> value })
    val res = attList.map(att => (att, dataWithLowercaseAttribs.count(record => record.getOrElse(att.toLowerCase(), null) == null)))
    res.toMap.filter(record => record._2 != 0)
  }

  /**
   * This function should extract a set of given attributes from a record
   *
   * @param record  Record that should be extracted
   * @param attList List of attributes that should be extracted
   * @return A Map that contains only the attributes that should be extracted
   *
   */
  def extractTrainingAttributes(record: Map[String, Any], attList: List[String]): Map[String, Any] = {
    // Ensure the attribute names in the record are lowercase for consistent matching
    val lowerCaseRecord = record.map { case (k, v) => k.toLowerCase -> v }

    // Map through each attribute and collect the value if it's not null or missing
    attList.flatMap { att =>
      val lowerAtt = att.toLowerCase // Convert attribute to lowercase to match record's keys
      lowerCaseRecord.get(lowerAtt).flatMap {
        case value: String if value.nonEmpty => Some(att -> value) // Ensure the value is not empty
        case value: Int => Some(att -> value) // Int values are directly added
        case value: Float => Some(att -> value.toInt) // Float values are casted to int
        case _ => None // Skip null, non-Int and empty values
      }
    }.toMap
  }

  /**
   * Helper function that categorizes the age attribute
   */
  def categorizeAge(age: Int): String = {
    if (age < 0) "Young Adult"
    else if (age <= 12) "Child"
    else if (age <= 19) "Teenager"
    else if (age <= 40) "Young Adult"
    else if (age <= 60) "Older Adult"
    else "Old"
  }

  /**
   * This function should create the training data set. It extracts the necessary attributes,
   * categorize them and deals with the missing values. You could find some hints in the description
   * and the lectures
   *
   * @param data Training Data Set that needs to be prepared
   * @return Prepared Data Set for using it with Naive Bayes
   */
  def createDataSetForTraining(data: List[Map[String, Any]]): List[Map[String, Any]] = {
    val necessaryAttributes = List("passengerID", "survived", "pclass", "sex", "age")
    data.map(record => {
      val extractedRecord = extractTrainingAttributes(record, necessaryAttributes)
      extractedRecord.updated("age", categorizeAge(extractedRecord.getOrElse("age", -1).asInstanceOf[Int]))
    })
  }


  /**
   * This function builds the model. It is represented as a function that maps a data record
   * and the name of the id-attribute to the value of the id attribute and the predicted class
   * (similar to the model building process in the train example)
   *
   * @param trainDataSet Training Data Set
   * @param classAttrib  name of the attribute that contains the class
   * @return A tuple consisting of the id (first element) and the predicted class (second element)
   */
  def createModelWithTitanicTrainingData(tdata: List[Map[String, Any]], classAttr: String):
  (Map[String, Any], String) => (Any, Any) = {
    val data = createDataSetForTraining(tdata).map(x => x - "passengerID")
    NaiveBayes.modelwithAddOneSmoothing(data, classAttr)
  }
}