module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4

    describe "Parte 1" $ do
        it "agrandar añade el ingrediente base al principio de la lista" $ do
            ingredientes (agrandar cuartoDeLibra) `shouldBe` [Carne, Pan, Carne, Cheddar, Pan]
  
        it "agrandar añade Carne si ya hay Carne en la hamburguesa" $ do
            ingredienteBase cuartoDeLibra `shouldBe` Carne
            head (ingredientes (agrandar cuartoDeLibra)) `shouldBe` Carne
  
        it "agrandar añade Pollo si hay Pollo y no hay Carne" $ do
            ingredienteBase (Hamburguesa 15 [Pan, Pollo, Cheddar, Pan]) `shouldBe` Pollo
            head (ingredientes (agrandar (Hamburguesa 15 [Pan, Pollo, Cheddar, Pan]))) `shouldBe` Pollo
  
        it "agrandar añade PatiVegano si hay PatiVegano y no hay Carne ni Pollo" $ do
            ingredienteBase (Hamburguesa 15 [Pan, PatiVegano, QuesoDeAlmendras, Pan]) `shouldBe` PatiVegano
            head (ingredientes (agrandar (Hamburguesa 15 [Pan, PatiVegano, QuesoDeAlmendras, Pan]))) `shouldBe` PatiVegano
  
        it "agrandar prioriza Carne si hay tanto Carne como Pollo" $ do
            ingredienteBase (Hamburguesa 15 [Pan, Carne, Pollo, Pan]) `shouldBe` Carne
            head (ingredientes (agrandar (Hamburguesa 15 [Pan, Carne, Pollo, Pan]))) `shouldBe` Carne
  
        it "agrandar dos veces añade el ingrediente base dos veces" $ do
            ingredientes (agrandar (agrandar cuartoDeLibra)) `shouldBe` [Carne, Carne, Pan, Carne, Cheddar, Pan]

        it "agregarIngrediente añade un ingrediente a la hamburguesa" $ do
            ingredientes (agregarIngrediente Panceta cuartoDeLibra) `shouldBe` [Panceta, Pan, Carne, Cheddar, Pan]
  
        it "descuento aplica el porcentaje correcto al precio base" $ do
            precioBase (descuento 20 cuartoDeLibra) `shouldBe` 16
  
        it "pdepBurger tiene la composición y precio correcto" $ do
            calcularPrecio pdepBurger `shouldBe` 110

    describe "Parte 2" $ do
        it "dobleCuarto tiene el precio correcto" $ do
            calcularPrecio dobleCuarto `shouldBe` 84
  
        it "bigPdep tiene el precio correcto" $ do
            calcularPrecio bigPdep `shouldBe` 89
  
        it "delDia aplica correctamente descuento y añade papas" $ do
            head (ingredientes (delDia dobleCuarto)) `shouldBe` Papas
            precioBase (delDia dobleCuarto) `shouldBe` precioBase dobleCuarto * 0.7
            calcularPrecio (delDia dobleCuarto) `shouldBe` 88

    describe "Parte 3" $ do
        it "hacerVeggie reemplaza ingredientes por sus versiones veganas" $ do
            Carne `notElem` ingredientes (hacerVeggie cuartoDeLibra) `shouldBe` True
            Panceta `notElem` ingredientes (hacerVeggie (agregarIngrediente Panceta cuartoDeLibra)) `shouldBe` True
            PatiVegano `elem` ingredientes (hacerVeggie cuartoDeLibra) `shouldBe` True
            QuesoDeAlmendras `elem` ingredientes (hacerVeggie cuartoDeLibra) `shouldBe` True
  
        it "agrandar una hamburguesa veggie le agrega un PatiVegano" $ do
            head (ingredientes (agrandar (hacerVeggie cuartoDeLibra))) `shouldBe` PatiVegano
  
        it "cambiarPanDePati reemplaza Pan por PanIntegral" $ do
            Pan `notElem` ingredientes (cambiarPanDePati cuartoDeLibra) `shouldBe` True
            PanIntegral `elem` ingredientes (cambiarPanDePati cuartoDeLibra) `shouldBe` True
  
        it "dobleCuartoVegano es un dobleCuarto veggie con pan integral" $ do
            PatiVegano `elem` ingredientes dobleCuartoVegano `shouldBe` True
            QuesoDeAlmendras `elem` ingredientes dobleCuartoVegano `shouldBe` True
            PanIntegral `elem` ingredientes dobleCuartoVegano `shouldBe` True
            Carne `notElem` ingredientes dobleCuartoVegano `shouldBe` True
            Pan `notElem` ingredientes dobleCuartoVegano `shouldBe` True