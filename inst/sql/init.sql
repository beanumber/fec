/* automatic table generation for CandCmteLinkage */;
 DROP TABLE IF EXISTS CandCmteLinkage;
 CREATE TABLE `CandCmteLinkage` (
  `cand_id` VARCHAR (9) NOT NULL,
  `cand_election_yr` INT (4) NOT NULL,
  `fec_election_yr` INT (4) NOT NULL,
  `cmte_id` VARCHAR (9),
  `cmte_tp` VARCHAR (1),
  `cmte_dsgn` VARCHAR (1),
  `linkage_id` INT (12) NOT NULL
)
;
 /* automatic table generation for CommitteeMaster */;
 DROP TABLE IF EXISTS CommitteeMaster;
 CREATE TABLE `CommitteeMaster` (
  `cmte_id` VARCHAR(9) NOT NULL,
  `cmte_nm` VARCHAR(200),
  `tres_nm` VARCHAR(90),
  `cmte_st1` VARCHAR(34),
  `cmte_st2` VARCHAR(34),
  `cmte_city` VARCHAR(30),
  `cmte_st` VARCHAR(2),
  `cmte_zip` VARCHAR(9),
  `cmte_dsgn` VARCHAR(1),
  `cmte_tp` VARCHAR(1),
  `cmte_pty_affiliation` VARCHAR(3),
  `cmte_filing_freq` VARCHAR(1),
  `org_tp` VARCHAR(1),
  `connected_org_nm` VARCHAR(200),
  `cand_id` VARCHAR(9)
)
;
 /* automatic table generation for CommitteetoCommittee */;
 DROP TABLE IF EXISTS CommitteetoCommittee;
 CREATE TABLE `CommitteetoCommittee` (
  `cmte_id` VARCHAR (9) NOT NULL,
  `amndt_ind` VARCHAR (1),
  `rpt_tp` VARCHAR (3),
  `transaction_pgi` VARCHAR (5),
  `image_num` VARCHAR(18),
  `transaction_tp` VARCHAR (3),
  `entity_tp` VARCHAR (3),
  `name` VARCHAR (200),
  `city` VARCHAR (30),
  `state` VARCHAR (2),
  `zip_code` VARCHAR (9),
  `employer` VARCHAR (38),
  `occupation` VARCHAR (38),
  `transaction_dt` DATE,
  `transaction_amt` DOUBLE (14,2),
  `other_id` VARCHAR (9),
  `tran_id` VARCHAR (32),
  `file_num` INT (22),
  `memo_cd` VARCHAR (1),
  `memo_text` VARCHAR (100),
  `sub_id` INT (19) NOT NULL
)
;
 /* automatic table generation for ContributionsbyIndividuals */;
 DROP TABLE IF EXISTS ContributionsbyIndividuals;
 CREATE TABLE `ContributionsbyIndividuals` (
  `cmte_id` VARCHAR (9) NOT NULL,
  `amndt_ind` VARCHAR (1),
  `rpt_tp` VARCHAR (3),
  `transaction_pgi` VARCHAR (5),
  `image_num` VARCHAR(18),
  `transaction_tp` VARCHAR (3),
  `entity_tp` VARCHAR (3),
  `name` VARCHAR (200),
  `city` VARCHAR (30),
  `state` VARCHAR (2),
  `zip_code` VARCHAR (9),
  `employer` VARCHAR (38),
  `occupation` VARCHAR (38),
  `transaction_dt` DATE,
  `transaction_amt` DOUBLE (14,2),
  `other_id` VARCHAR (9),
  `tran_id` VARCHAR (32),
  `file_num` INT (22),
  `memo_cd` VARCHAR (1),
  `memo_text` VARCHAR (100),
  `sub_id` INT (19) NOT NULL
)
;
 /* automatic table generation for ContributionstoCandidates */;
 DROP TABLE IF EXISTS ContributionstoCandidates;
 CREATE TABLE `ContributionstoCandidates` (
  `cmte_id` VARCHAR (9) NOT NULL,
  `amndt_ind` VARCHAR (1),
  `rpt_tp` VARCHAR (3),
  `transaction_pgi` VARCHAR (5),
  `image_num` VARCHAR(18),
  `transaction_tp` VARCHAR (3),
  `entity_tp` VARCHAR (3),
  `name` VARCHAR (200),
  `city` VARCHAR (30),
  `state` VARCHAR (2),
  `zip_code` VARCHAR (9),
  `employer` VARCHAR (38),
  `occupation` VARCHAR (38),
  `transaction_dt` DATE,
  `transaction_amt` DOUBLE (14,2),
  `other_id` VARCHAR (9),
  `cand_id` VARCHAR (9),
  `tran_id` VARCHAR (32),
  `file_num` INT (22),
  `memo_cd` VARCHAR (1),
  `memo_text` VARCHAR (100),
  `sub_id` INT (19) NOT NULL
)
;
 /* automatic table generation for OperatingExpenditures */;
 DROP TABLE IF EXISTS OperatingExpenditures;
 CREATE TABLE `OperatingExpenditures` (
  `cmte_id` VARCHAR (9) NOT NULL,
  `amndt_ind` VARCHAR (1),
  `rpt_yr` INT(4),
  `rpt_tp` VARCHAR (3),
  `image_num` VARCHAR(18),
  `line_num` TEXT,
  `form_tp_cd` VARCHAR (8),
  `sched_tp_cd` VARCHAR (8),
  `name` VARCHAR (200),
  `city` VARCHAR (30),
  `state` VARCHAR (2),
  `zip_code` VARCHAR (9),
  `transaction_dt` DATE,
  `transaction_amt` DOUBLE (14,2),
  `transaction_pgi` VARCHAR (5),
  `purpose` VARCHAR (100),
  `category` VARCHAR (3),
  `category_desc` VARCHAR (40),
  `memo_cd` VARCHAR (1),
  `memo_text` VARCHAR (100),
  `entity_tp` VARCHAR (3),
  `sub_id` INT (19) NOT NULL,
  `file_num` INT (7),
  `tran_id` VARCHAR (32) NOT NULL,
  `back_ref_tran_id` VARCHAR (32)
)
;
