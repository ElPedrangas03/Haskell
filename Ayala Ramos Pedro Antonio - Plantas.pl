planta(belladona).
planta(berro).
planta(boldo).

nombre_cientifico(belladona,'atropa belladona').
nombre_cientifico(berro,'nasturtium off').
nombre_cientifico(boldo,'pneumus boldo').

%belladona
propiedad(antiepileptica).

%berro
propiedad(desinflama).
propiedad(descongestiona).

%boldo
propiedad(depurativo).
propiedad(digestivo).

%belladona
planta_alivia(belladona,'tos ferina').
planta_alivia(belladona,'epilepsia').
planta_alivia(belladona,'colicos hepaticos').
planta_alivia(belladona,'gastralgia').

%berro
planta_alivia(berro,'desinflama las anginas').
planta_alivia(berro,'dolor estomacal').
planta_alivia(berro, 'escorbuto').
planta_alivia(berro, 'anemia').
planta_alivia(berro, 'sifilis').

%boldo
planta_alivia(boldo,'calculos biliares').

%belladona
forma_empleo('tos ferina', 'consulte a su medico').
forma_empleo('epilepsia', 'consulte a su medico').
forma_empleo('colicos hepaticos', 'consulte a su medico').
forma_empleo('gastralgia', 'consulte a su medico').

%berro
forma_empleo('desinflama las anginas', 'se come crudo').
forma_empleo('dolor estomacal', 'se come crudo').
forma_empleo('escorbuto', 'se come crudo').
forma_empleo('anemia', 'se come crudo').
forma_empleo('sifilis', 'se come crudo').

%boldo
forma_empleo('calculos biliares', 'Se utilizan las hojas').


forma_tratamiento(Planta, Empleo, Propiedad) :-
    planta_alivia(Planta, Propiedad),
    forma_empleo(Propiedad, Empleo).
