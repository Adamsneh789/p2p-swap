P2P-Swap
A peer-to-peer swap smart contract built with Clarity on the Stacks blockchain.
It enables trustless, atomic swaps of STX and SIP-010 fungible tokens between two users.

Features
Create swap offers with defined terms
Accept swaps securely and atomically
Cancel open offers before they’re taken
Event logs for swap creation, acceptance, and cancellation
Supports STX and SIP-010 fungible tokens

Technical Overview
Language: Clarity
Core Functions:
create-offer – define assets to give and receive
accept-offer – fulfill offer atomically
cancel-offer – withdraw open offer
get-offer-info – check details of a swap
