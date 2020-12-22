package adventofcode.year2020.day21

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.io.Source

object Day21 {
  def main(args: Array[String]): Unit = {
    run("src/adventofcode/year2020/day21/inputExample.txt")
    run("src/adventofcode/year2020/day21/input.txt")


  }

  type Ingredient = String
  type Allergen = String
  type Rule = (Set[Ingredient], Set[Allergen], Int)

  def run(inputFile: String) = {
    val foods: Seq[Rule] = Source.fromFile(inputFile).getLines().toSeq.zipWithIndex
      .map(parseRule(_))
    val allIngredients = foods.flatMap(_._1).toSet
    val allAllergens = foods.flatMap(_._2).toSet
//    val allergenToIngredient = rules.foldLeft(Map.empty[String, Set[String]])((acc, rule) => {
//      val(ingredients, allergens) = rule
//      allergens.foldLeft(acc)((acc2, allergen)=>{
//        val existing = acc2.getOrElse(allergen, Set.empty[String])
//        acc2.updated(allergen, existing ++ ingredients)
//      })
//    })
//
//    val ingredienttoAllergen = rules.foldLeft(Map.empty[String, Set[String]])((acc, rule) => {
//      val(ingredients, allergens) = rule
//      ingredients.foldLeft(acc)((acc2, ingredient)=>{
//
//        val existing = acc2.getOrElse(ingredient, Set.empty[String])
//        acc2.updated(ingredient, existing ++ allergens)
//      })
//    })
//
//    println(s"Total ingredients are ${allIngredients.size}")
//    println(s"Total allergens are ${allergenToIngredient.size}")
//    println(s"Total combinations are ${allergenToIngredient.map(_._2.size).mkString("*")} =  ${allergenToIngredient.map(_._2.size.toLong).reduce(_ * _)}")
//    println(s"Total combinations are ${ingredienttoAllergen.map(_._2.size).mkString("*")} =  ${ingredienttoAllergen.map(_._2.size.toLong).reduce(_ * _)}")

    // assign every known allergen to every known ingredient
    val ingredientAllergenMapping = allIngredients.flatMap(ingredient => {
      allAllergens.map(allergen => (ingredient, allergen))
    })

    // if an allergen is in any food that the ingredient isn't then that ingredient doesn't have the allergen
    // so check the foods that the allergen is in, and match it against the foods that the ingredients is in
    // if there are foods that are in allergen's list but not in ingredient's list, then discard that mapping
    val possibleIngredientAllergen = ingredientAllergenMapping.filter(ia => {
      val (ingredient, allergen) = ia
      val ingredientFoods = foods.filter(food => food._1.contains(ingredient)).map(_._3).toSet
      val allergenFoods = foods.filter(food => food._2.contains(allergen)).map(_._3).toSet
      allergenFoods.filter(!ingredientFoods.contains(_)).size==0
    }).groupBy(_._2).mapValues(_.map(_._1))

    println(possibleIngredientAllergen.map(ai => s"${ai._1} -> ${ai._2.mkString(",")}").mkString("\n"))

    //seperate allergens that have only one ingredient from allergens that have multiple ingredients
    // then remove the seperated ingredients from the allergens that have those ingredients.. repeat till no more allergens left
    val finalAllergenIngredients = cull(Map.empty[Allergen,Ingredient], possibleIngredientAllergen)
    println(finalAllergenIngredients.map(ai => s"${ai._1} -> ${ai._2}").mkString("\n"))

    val badIngredients = finalAllergenIngredients.map(_._2).toSet
    val goodIngredients = allIngredients.filter(!badIngredients.contains(_))

    println(foods.flatMap(_._1.toSeq).filter(goodIngredients.contains(_)).size)
    println(finalAllergenIngredients.toSeq.sortBy(_._1).map(_._2).mkString(","))
  }

  @tailrec
  def cull(resolved: Map[Allergen, Ingredient], allergenToIngredients: Map[Day21.Allergen, Set[Day21.Ingredient]]): Map[Allergen, Ingredient] = {
    if(allergenToIngredients.isEmpty) {
      resolved
    } else {
      val singleIngredientAllergens = allergenToIngredients.filter(_._2.size == 1).map(ai => ai._1 -> ai._2.head)
      if(singleIngredientAllergens.isEmpty) {
        throw new Exception("COuldn't find allergens with only ingredient")
      }
      val resolvedIngredients = singleIngredientAllergens.map(_._2).toSet
      val remainingAllergens = allergenToIngredients.filter(_._2.size > 1).mapValues(_.filter(!resolvedIngredients.contains(_)))
      cull(resolved ++ singleIngredientAllergens, remainingAllergens)
    }

  }

  def parseRule(line: (String, Int)) :Rule = {
    //mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    val ruleFormat = """(.*) \(contains (.*)\)""".r
    line match {
      case (ruleFormat(ing, all), n) => (ing.split(" ").toSet, all.split(", ").toSet, n)
      case _ => throw new Exception(s"Can't parse $line")
    }
  }
}
