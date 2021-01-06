
module mutt

greet() = println("Hello World!")

export Party, AbstractParty, AssetType, Transaction
println("Hello")
module Data
using UUIDs
@enum PartyType begin
    currencyIssuer = 1
    currencyUser = 2
    foreignCurrencyIssuer = 3
    foreignCurrencyUser = 4
end # PartyType

abstract type AbstractParty end

abstract type AssetType end

mutable struct Transaction{T<:AbstractParty}
    id::UUID
    description::String
    amount::Int64
    type::AssetType
    from::T
    to::T
    function Transaction(from, to, amount, type, description)
        x = new{T}()
        x.id = uuid4()
        x.from = from
        x.to = to
        x.amount = amount
        x.type = type
        x.description = description
        return x
    end
end # Transaction

mutable struct Party <: AbstractParty
    id::UUID
    name::String
    transactions::Vector{Transaction}
    balance::Int64
    type::PartyType
    function Party(name, type)
        x = new()
        x.id = uuid4()
        x.transactions = []
        x.type = type
        x.balance = 0
        x.name = name
        return x
    end
end # Party

end
println("end")
greet()
end # module
