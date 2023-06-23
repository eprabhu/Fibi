package com.polus.fibicomp.coi.dto;

import lombok.Getter;
import lombok.Setter;

import java.sql.Timestamp;

@Getter
@Setter
public class PersonEntityDto {

	private Integer personEntityId;
	private Integer personEntityNumber;
	private String personId;
	private Boolean isRelationshipActive;
	private Integer versionNumber;
	private String versionStatus;
	private Timestamp updateTimestamp;
	private  String personFullName;
	private String revisionReason;
	
}
