module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | PanIntegral | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente PanIntegral = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 0

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {
    ingredientes = ingrediente : ingredientes hamburguesa
}

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase hamburguesa
    | Carne `elem` ingredientes hamburguesa = Carne
    | PatiVegano `elem` ingredientes hamburguesa = PatiVegano
    | otherwise = Pollo

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (ingredienteBase hamburguesa) hamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento unDescuento hamburguesa = hamburguesa {
    precioBase = precioBase hamburguesa * (1 - (unDescuento / 100))
}

calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))

pdepBurger :: Hamburguesa
pdepBurger =
    descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar $ cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngrediente Papas $ hamburguesa

cambiarPorVeggie :: Ingrediente -> Ingrediente
cambiarPorVeggie Carne = PatiVegano
cambiarPorVeggie Pollo = PatiVegano
cambiarPorVeggie Cheddar = QuesoDeAlmendras
cambiarPorVeggie Panceta = BaconDeTofu
cambiarPorVeggie ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {
    ingredientes = map cambiarPorVeggie (ingredientes hamburguesa)
}

cambiarPorPanIntegral :: Ingrediente -> Ingrediente
cambiarPorPanIntegral Pan = PanIntegral
cambiarPorPanIntegral ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa {
    ingredientes = map cambiarPorPanIntegral (ingredientes hamburguesa)
}

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie . cambiarPanDePati $ dobleCuarto