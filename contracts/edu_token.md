# Token for certification
> Genrated with [Archetype](https://archetype-lang.org/) v0.1.12

## Assets

### Learner
A learner belongs to an institution (see Institution asset below), passes certifications (see Certification asset below), and receives tokens when certified (see Learner_token asset below).

A learner is identified by a blockchain account address.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| lid | Learner identifier | role |

### Institution
An institution requests its learners to be certified. For example the institution is a university or a company. When a university, the learner is a student; when a company, the learner is an employee sent to certification.

The institution receives tokens when one of its learners is sent to certification and when one of its learners is certified (see Institution_tokens asset below).

An institution is identified by a blockchain account address.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| iid | Institution identifier | address |
| ipubk | Public key for message encryption (not used yet) | string |

### Certifier
A certifier certifies a learner for a diploma (see Diploma asset below).

A certifier is identified by a blockchain account address.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| ccid | Certifier identifier | address |


### Certificate
A certificate is delivered to a learner when certified.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| did | Diploma identifier | string |
| dtkl | Number of tokens received by the learner when certified | int |
| dtki | Number of tokens received by the learner's institution when learner is certified | int |

### Certification
A certification is created when a learner is certified. It comprises the following information:
* date of certification
* certification identifier
* learner identifier
* learner's institution identifier

| Field | Desc | Type | Attribute |
|--|--|--|--|
| cid | Certification identifier | string |
| cdate | Date of certification | date |
| cdip | Certificate id | pkey of certificate |
| ccer | Learner id | pkey of learner |
| ciid | Institution id | pkey of institution |

### Institution_learners
Association table between an institution and its learners.

Note that a learner may belong to several institutions.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| ilid | Institution identifier | pkey of institution |
| illearners | Collection of its learners | learner collection |

### Institution_tokens
Number of tokens received by an institution.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| itid | Institution identifier | pkey of institution |
| ittokens | Number of tokens received by the institution | int |


### Learner_tokens
Number of tokens received by a learner.

| Field | Desc | Type | Attribute |
|--|--|--|--|
| ltid | Learner identifier | pkey of learner |
| ltokens | Number of tokens received | int |

### Other_token_holder
Tokens received either by institutions or learners may be transferred to other token holders in exchange of goods and/or services For example, a learning platform may give goodies to learners in exchange of tokens.

This asset registers the number of tokens held by other entities (other than institution, learner or certifier).

| Field | Desc | Type | Attribute |
|--|--|--|--|
| othid | other entity identifier | address |
| oth_learner_tokens | Number of learners' tokens held | int |
| oth_institution_tokens | Number of institutions' tokens held  | int |

## Actions

### Add learner
The `add_learner` action is used by a learner to register in the system.

#### Fail if

The call fails if the caller is already registered (cannot register twice).

##### f1 `(learner.contains(caller))`

### Register learners
The `register_learners` is used by an institution to register (or unregister) a list of learners.

| Name | Desc | Type |
|--|--|--|
|learners|Collection of learners to process |learner collection|
|do_add| register learners if `true`; *unregister* if `false` |bool|
#### Require

This call requires the caller to be an institution (fails otherwise).

##### r0 `(institution.contains(caller))`

### Certify
The `certify` call is used by a certifier to certify a list of learners.

Note that the certification data is provided as such to the contract and is thus created off-chain. This call however checks that the data is consistent with respect to the association between learner and institution.

It generates the corresponding tokens for learners and institutions.

| Name | Desc | Type |
|--|--|--|
|certified| Certifications to register |certification collection|
#### Require

This call requires the caller to be a certifier (fails otherwise).
##### r1 `(certifier.contains(caller))`

### Transfer learner token

This call is used by the learner to transfer tokens to another token hodler.

| Name | Desc | Type |
|--|--|--|
|nb_tokens| Number of tokens to tranfer|int|
|dest| Address to transfer tokens to |address|
#### Require

This call requires to be called by a learner with enough tokens, and that the recipient exists in the collection of other token holders.

##### r2 `(learner_tokens.contains(caller))`
##### r3 `((learner_tokens.get(caller)).ltokens >= nb_tokens)`
##### r4 `(other_token_holder.contains(dest))`

### Transfer institution token

This call is used by the institution to transfer tokens to another token hodler.

| Name | Desc | Type |
|--|--|--|
|nb_tokens|Number of tokens to tranfer|int|
|dest|Address to transfer tokens to|address|
#### Require
This call requires to be called by an institution with enough tokens, and that the recipient exists in the collection of other token holders.

##### r5 `(institution_tokens.contains(caller))`
##### r6 `((institution_tokens.get(caller)).ittokens >= nb_tokens)`
##### r7 `(other_token_holder.contains(dest))`

