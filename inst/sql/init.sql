/* automatic table generation for cand_com_link */;
 DROP TABLE IF EXISTS cand_com_link;
 CREATE TABLE `cand_com_link` (
  `cand_id` VARCHAR (9) NOT NULL,
  `cand_election_yr` INT (4) NOT NULL,
  `fec_election_yr` INT (4) NOT NULL,
  `cmte_id` VARCHAR (9),
  `cmte_tp` VARCHAR (1),
  `cmte_dsgn` VARCHAR (1),
  `linkage_id` INT (12) NOT NULL
)
;
 /* automatic table generation for candidates */;
 DROP TABLE IF EXISTS candidates;
 CREATE TABLE `candidates` (
  `cand_id` VARCHAR(9) NOT NULL,
  `cand_name` VARCHAR(200),
  `cand_pty_affiliation` VARCHAR(3),
  `cand_election_yr` INT(4),
  `cand_office_st` VARCHAR(2),
  `cand_office` VARCHAR(1),
  `cand_office_district` VARCHAR(2),
  `cand_ici` VARCHAR(1),
  `cand_status` VARCHAR(1),
  `cand_pcc` VARCHAR(9),
  `cand_st1` VARCHAR(34),
  `cand_st2` VARCHAR(34),
  `cand_city` VARCHAR(30),
  `cand_st` VARCHAR(2),
  `cand_zip` VARCHAR(9)
)
;
 /* automatic table generation for committees */;
 DROP TABLE IF EXISTS committees;
 CREATE TABLE `committees` (
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
 /* automatic table generation for contrib_com_to_cand */;
 DROP TABLE IF EXISTS contrib_com_to_cand;
 CREATE TABLE `contrib_com_to_cand` (
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
 /* automatic table generation for contrib_com_to_com */;
 DROP TABLE IF EXISTS contrib_com_to_com;
 CREATE TABLE `contrib_com_to_com` (
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
 /* automatic table generation for contrib_indiv_to_com */;
 DROP TABLE IF EXISTS contrib_indiv_to_com;
 CREATE TABLE `contrib_indiv_to_com` (
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
 /* automatic table generation for expenditures */;
 DROP TABLE IF EXISTS expenditures;
 CREATE TABLE `expenditures` (
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
 DROP TABLE IF EXISTS `house_elections`;
 CREATE TABLE `house_elections` (
  `cand_id` varchar(9) NOT NULL default '',
  `state` varchar(2) NOT NULL default '',
  `district` varchar(25) NOT NULL default '',
  `incumbent` varchar(5) NOT NULL default '',
  `candidate_name` varchar(255) NOT NULL default '',
  `party` varchar(1) NOT NULL default '',
  `primary_votes` int NOT NULL default 0,
  `runoff_votes` int NOT NULL default 0,
  `general_votes` int NOT NULL default 0,
  `ge_winner` varchar(1) NOT NULL default '',
  `election_cycle` smallint(4) DEFAULT NULL
)
 ;