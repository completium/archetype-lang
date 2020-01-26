pragma solidity >=0.5.0 <0.7.0;

contract erc20_ref {
    string public symbol = "erc20_ref";
    mapping(address => uint256) public balances;

    constructor() public {
        balances[msg.sender] = 1000;
    }

    function attributeTokens(address _to, uint256 _amount) public {
        balances[_to] += _amount;
    }

    function decreaseMaxTokens(uint256 _amount) public {
        if (balances[msg.sender] < _amount) {
            revert("fail");
        }
        balances[msg.sender] -= _amount;
    }

    function transfer(address _to, uint256 _amount) public {
        if (balances[msg.sender] < _amount) {
            revert("fail");
        }
        balances[msg.sender] -= _amount;
        balances[_to] += _amount;
    }

    function balanceOf(address tokenOwner)
        public
        view
        returns (uint256 balance)
    {
        return balances[tokenOwner];
    }
}
