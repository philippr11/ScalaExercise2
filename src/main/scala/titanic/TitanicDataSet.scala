package titanic

import scala.util.{Try, Success, Failure}

object TitanicDataSet {

  /**
   * Creates a model that predicts 1 (survived) if the person of the certain record
   * is female and 0 (deceased) otherwise
   *
   * @return The model represented as a function
   */
  def simpleModel: (Map[String, Any], String) => (Any, Any) = ???

  /**
   * This function should count for a given attribute list, how often an attribute is
   * not present in the data records of the data set
   *
   * @param data    The DataSet where the counting takes place
   * @param attList List of attributes where the missings should be counted
   * @return A Map that contains the attribute names (key) and the number of missings (value)
   */
  def countAllMissingValues(data: List[Map[String, Any]], attList: List[String]): Map[String, Int] = {
    val res = attList.map(att => (att, data.count(record => record.getOrElse(att, null) == null)))
    res.toMap
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
      val lowerAtt = att.toLowerCase  // Convert attribute to lowercase to match record's keys
      lowerCaseRecord.get(lowerAtt).flatMap {
        case value: String if value.nonEmpty => Some(att -> value)  // Ensure the value is not empty
        case value: Int => Some(att -> value)  // Int values are directly added
        case _ => None  // Skip null, non-Int and empty values
      }
    }.toMap
  }

  /**
   * Helper function that categorizes the age attribute
   */
  def categorizeAge(age: Option[Any]): String = age match {
    case Some(a: Double) if a <= 12 => "Child" // Assuming age might be parsed as Double
    case Some(a: Double) if a <= 19 => "Teenager"
    case Some(a: Double) if a <= 40 => "Young Adult"
    case Some(a: Double) if a <= 60 => "Older Adult"
    case Some(a: Double) => "Old"
    case _ => "Unknown" // Handle non-numeric or missing age gracefully
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
    val necessaryAttributes = List("PassengerId", "Survived", "Pclass", "Sex", "Age")

    data.map { record =>
      val extractedAttributes = extractTrainingAttributes(record, necessaryAttributes)
      val updatedAttributes = necessaryAttributes.foldLeft(extractedAttributes) { (acc, att) =>
        acc.get(att) match {
          case Some(value) => acc // if value is present, keep it
          case None => acc - att // if value is missing, ensure it's not in the map
        }
      }
      updatedAttributes.updated("Age", categorizeAge(updatedAttributes.get("Age")))
    }
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
  (Map[String, Any], String) => (Any, Any) = ???
}