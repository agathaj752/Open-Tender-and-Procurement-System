# 🏛️ Open Tender Smart Contract System

A transparent and decentralized tender management system built on Stacks blockchain.

## 🎯 Features

- Create public tenders with detailed specifications
- Submit competitive bids with proposals
- Transparent bid management
- Secure winner selection
- Immutable tender history

## 🔧 Contract Functions

### For Tender Creators

- `create-tender`: Create a new tender with title, description, deadline, and minimum bid
- `close-tender`: Close an active tender
- `select-winner`: Select winning bid for a closed tender

### For Bidders

- `submit-bid`: Submit a bid with amount and proposal
- `get-bid`: View specific bid details
- `get-tender`: View tender details

## 📝 Usage Example

1. Create a new tender:
```clarity
(contract-call? .open-tender create-tender "Road Construction" "Build 5km highway" u100 u1000000)
```

2. Submit a bid:
```clarity
(contract-call? .open-tender submit-bid u1 u1500000 "Proposal details here")
```

3. Close tender:
```clarity
(contract-call? .open-tender close-tender u1)
```

4. Select winner:
```clarity
(contract-call? .open-tender select-winner u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔒 Security

- Only tender owner can close tender and select winner
- Bids must meet minimum amount requirement
- Automatic deadline enforcement
- Immutable bid history

## 🚀 Getting Started

1. Clone the repository
2. Install Clarinet
3. Run `clarinet console`
4. Deploy contract
5. Start interacting with the contract

## 📜 License

MIT License


