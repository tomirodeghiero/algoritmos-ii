module addressBook


sig Name{}
sig Address{}

sig AddressBook{
    names: set Name,
    addr:names -> Address
}

// Asegura que solo los nombres en 'names' tengan direcciones en 'addr'
fact  domAddrIsNames{
	all b:AddressBook | b.addr.Address = b.names
}

// Asegura que cada nombre tenga a lo sumo una dirección
fact addrIsFuncional{
	all b: AddressBook | all n: b.names | one n.(b.addr)
}

// Agrega una dirección para un nombre
pred addAddress[b: AddressBook, n: Name, a: Address, bp: AddressBook] {
    bp.names = b.names + n
    bp.addr = b.addr + (n -> a)
}

// Consulta la dirección de un nombre
pred lookupAddress[b: AddressBook, n: Name, a: Address] {
    n in b.names
    a = n.(b.addr)
}

// Elimina la dirección de un nombre
pred removeAddress[b: AddressBook, n: Name, bp: AddressBook] {
    bp.names = b.names - n
    bp.addr = b.addr - n->(n.(b.addr))
}

// Asegura que una persona eliminada ya no está en la agenda
assert removedPersonNotInBook {
    all b: AddressBook, n: Name, bp: AddressBook | 
        removeAddress[b, n, bp] => n not in bp.names
}

// Asegura que agregar y luego eliminar una dirección vuelve a la agenda original
assert addThenRemoveReturnsOriginal {
    all b: AddressBook, n: Name, a: Address, bp, bp2: AddressBook | 
        addAddress[b, n, a, bp] and removeAddress[bp, n, bp2] => 
        b.names = bp2.names and b.addr = bp2.addr
}

// Asegura que sobrescribir una dirección pierde el primer valor
assert overwriteAddressLosesOriginal {
    all b: AddressBook, n: Name, a1, a2: Address, bp: AddressBook |
        addAddress[b, n, a1, bp] and addAddress[bp, n, a2, bp] => 
        n.(bp.addr) = a2 and a1 != a2
}

pred show[] {}

run show for 3 but 3 Name, 3 Address, 3 AddressBook
