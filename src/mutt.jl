
module mutt

greet() = println("Hello World!")

println("Hello")
module Data
using UUIDs
using SHA
export Party, AbstractParty, AssetType, Transaction
@enum PartyType begin
    currencyIssuer = 1
    currencyUser = 2
    foreignCurrencyIssuer = 3
    foreignCurrencyUser = 4
end # PartyType

abstract type AbstractParty end

abstract type AssetType end

struct Transaction{T <: AbstractParty}
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
mutable struct PartyState
    _balance::Int64
    _sha::Array{UInt8,1}
    _transactions::Vector{Transaction}
end

function addTransaction(p::PartyState, trans::Transaction)
    ctx = SHA2_256_CTX()
    update!(ctx, p._sha) 
    update!(ctx, Vector{UInt8}(string(trans)))
end


struct Party <: AbstractParty
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

module Runner
using UUIDs
using ..Data
using Match

"""
A message that can be sent to a party's channel
"""
abstract type Message end
struct EndListening <: Message
end

partyLock = Threads.Condition()
parties = Dict{UUID,Tuple{Any,Channel{Message}}}()
function _doRealRun(party, c::Channel{Message}) 
    println("In do real run")
    while true
        msg = take!(c)
        println("Got message ", msg)
        @match msg begin
            EndListening() => begin
                lock(partyLock)
                try
                    delete!(parties, party.id)
                    return
                finally
                    unlock(partyLock)
                end
            end
            unknown => begin
                println("Got unknown message", unknown)
            end
        end
    end
end

function startTask(party)::Channel{Message}
    lock(partyLock)
    try
        uuid = party.id
        println("uuid is ", uuid)
        println("parites ", parties)
        got = get(parties, uuid, nothing)
        println("Got: ", got)
        if got === nothing begin 
                
                function runit(c::Channel{Message})
                    println("Start runit")
                    _doRealRun(party, c)
                    println("End runnit")
                end
                chan = Channel{Message}(runit)
                push!(parties, uuid => (party, chan))
                println("Parties", parties)
                return chan
            end
        else begin 
                return got[2]
            end
        end        
        finally
       unlock(partyLock) 
    end
end

end # Runner

println("end")
greet()
end # module
