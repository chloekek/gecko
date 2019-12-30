ADT syntax
==========

GECKO-C has wrappers, extensible records, and polymorphic variants, but it
does not have user-defined algebraic data types.

GECKO-AS therefore supplies the following syntax to make the definition of
ADTs more convenient::

    adt list
    of a : type
    case nil of {}
    case cons of {head : a, tail : list a}
    end

This desugars into a wrapper around a variant as well as a constructor
function for each variant. The constructor functions do wrap and inject so
you don’t have to.

.. todo::

   Consider generating optics rather than constructor functions.
