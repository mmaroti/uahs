
module Test where

data Shape
	= ShapeOne
	| ShapeProd Int Shape
	| ShapeSum [Shape]
