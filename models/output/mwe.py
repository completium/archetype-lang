import smartpy as sp


class Mwe(sp.Contract):
    def __init__(self):
    self.init(admin=sp.address("tz1aazS5ms5cbGkb6FN1wvWmN7yrMTTcr6wB"),
              mile_keys=[],
              mile_assets={},
              owner_keys=[],
              owner_assets={})

    # API function

    def get_mile(self, key):
        self.data.mile_assets[key]

    def set_mile(self, key, asset):
        self.data.mile_assets[key] = asset

    def add_mile(self, asset):
        key = asset.id
        self.data.mile_keys.append(key)
        self.data.mile_assets[key] = asset

    def remove_mile(self, key):
        self.data.mile_keys.remove(key)
        del self.data.mile_assets[key]

    def select_mile(s, c, p):
        []

    def sum_mile_amount(self):
        reduce((lambda x, key: x[key].amount + y), s.mile_assets, 0)

    def get_owner(self, key):
        self.data.owner_assets[key]

    def add_owner(self, asset):
        key = asset.addr
        self.data.owner_keys.append(key)
        self.data.owner_assets[key] = asset

    def add_owner_miles(s, asset, b):
        asset = asset.miles.insert(b)
        self.data.owner_assets[asset.addr] = asset

    def remove_owner_miles(s, asset, key):
        asset = asset.miles.pop(key)
        self.data.owner_assets[asset.addr] = asset

    def contains_owner(l, key):
        key in l

    # Functions

    def add_shallow_owner(self, owner, owner_miles):
        add_owner(self, owner)
        sp.for mile in owner_miles:
            add_mile(self, mile)

    @sp.entryPoint
    def add(self, params):
        sp.verify(not (sp.sender=self.data.admin))
        sp.verify(not (sp.amount=sp.tez(0)))
        sp.verify(not (newmile.amount > 0))
        sp.if (contains_owner(self.data.owner_keys, ow)):
            add_owner_miles(self, get_owner(self, ow), newmile)
        sp.else:
            add_shallow_owner(
                sp.Record(addr=ow, miles=[newmile.id]), [newmile])

    @sp.entryPoint
    def consume(self, params):
        sp.verify(not (sp.sender=self.data.admin))
        sp.verify(not (sp.amount=sp.tez(0)))
        ow = get_owner(self, a)
        by_expiration = select_mile(self, ow.miles, fun the -> the.expiration > sp.currentTime)
        sp.verify(not (sum_mile_amount(self) >= quantity))
        remainder = quantity
        sp.for m in by_expiration:
            sp.if (remainder > 0):
                sp.if (m.amount > remainder):
                    remainder = 0
                    _k = m.id
                    _mile = get_mile(self, _k)
                    _mile = sp.Record(id=_mile.id, amount=_mile.amount -
                                      remainder, expiration=_mile.expiration)
                    set_mile(self, _k, _mile)
                sp.else:
                    sp.if (m.amount=remainder):
                    remainder = 0
                    remove_owner_miles(self, ow, m.id)
            sp.else:
        remainder -= m.amount
        remove_owner_miles(self, ow, m.id)

    @sp.entryPoint
    def clear_expired(self, params):
        sp.verify(not (sp.sender=self.data.admin))
        sp.verify(not (sp.amount=sp.tez(0)))
        sp.for o in self.data.owner_keys:
            _assets = select_mile(self, o.miles, fun the -> the.expiration < sp.currentTime)
        sp.for _mile in _assets:
            remove_mile(self, _mile.id)


# Tests
@addTest(name="test")
def test():
          # define a contract
    c1 = Mwe()
    # show its representation
    html = c1.fullHtml()
    setOutput(html)
