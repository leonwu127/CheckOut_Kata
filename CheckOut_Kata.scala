object CheckOut {
    def main(args: Array[String]) = {
        println("Test price on original rules!")
        val priceRules ="""
        A,50,3 for 130
        B,30,2 for 45
        C,20
        D,15
        """
        val ruleMap = RuleEngine.genRuleMap(priceRules)

        // Testing process
        // Inline testing
        var result = true
        result = result && (RuleEngine.calTotalPrice(ruleMap, List()) == 0) // empty test
        result = result && (RuleEngine.calTotalPrice(ruleMap, List("A","A","B","B","C","A")) == 195) // with special price test
        result = result && (RuleEngine.calTotalPrice(ruleMap, List("A","B","C","D")) == 115) // with unit price test
        
        
        if(result){
            println("All test pass!")
        }else{
            println("Test failed!")
        }
        
        println("Test price on new rules!")
        val priceRules_2 ="""
        A,50,3 for 130
        B,30,2 for 45
        C,20,2 for 30
        D,15
        """
        val ruleMap_2 = RuleEngine.genRuleMap(priceRules_2)
        
        // Testing process
        // Inline testing
        result = result && (RuleEngine.calTotalPrice(ruleMap_2, List("A","A","B","C","B","C","A")) == 205) // with special price test
        
        if(result){
            println("All test pass!")
        }else{
            println("Test failed!")
        }
        
    }
    
    object RuleEngine {
    // Singleton rule engine involves rule map and price calculation function.
    // Rule trait to define shared abstract methods between different price rule.
    trait Rule {
        def calItemTotalPrice(count: Int): Double
    }
    
    // Normal Price Rule
    private case class NormalPrice(unitPrice: Double) extends Rule{
        def calItemTotalPrice(count: Int) = count * unitPrice
    }
    
    // Special Price Rule
    private case class SpecialPrice(unitPrice: Double, n: Int, specialPrice: Double) extends Rule{
        def calItemTotalPrice(count: Int) = count / n * specialPrice + count % n * unitPrice
    }
    
    // Generate item -> rule pair
    def genItemRulePair(ruleString: String): (String, Rule) = {
        val specialPrice = """(.+) for (.+)$""".r
        ruleString.split(",") match{
            // key -> value pair
            case Array(item, unitPrice) => item -> NormalPrice(unitPrice.toDouble)
            case Array(item, unitPrice, specialPrice(n, specialPrice)) => item -> SpecialPrice(unitPrice.toDouble, n.toInt, specialPrice.toDouble)
            case _ => throw new Exception(s"Wrong rule!")
        }
    }
    
    // Generate rule maps
    def genRuleMap(priceRules: String): Map[String, Rule] = {
        val rulePairs = for {
            rules <- priceRules.split("\n")
            trim = rules.trim if (!trim.isEmpty())
            rulePair = genItemRulePair(trim)
        } yield rulePair
        rulePairs.toMap
    }
    
    // Calculate total price
    def calTotalPrice(ruleMap: Map[String, Rule], items: List[String]): Double = {
        // item -> count maps
        val count = items.groupBy(identity).mapValues(_.size)
        val itemSumArray = for{
            (item, n) <- count
            rule = ruleMap.getOrElse(item, throw new Exception("Error 404: Rule not found"))
        }yield rule.calItemTotalPrice(n)
        itemSumArray.sum
    }
}
}
