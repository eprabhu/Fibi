package com.polus.fibicomp.coi.dto;


import lombok.Getter;
import lombok.Setter;
import java.sql.Timestamp;
import java.util.Date;

@Getter
@Setter
public class PersonEntityDto {

	private Integer personEntityId;
	private Integer personEntityNumber;
	private String personId;
	private Integer entityId;
	private Integer entityNumber;
	private Boolean isRelationshipActive;
	private Integer versionNumber;
	private String versionStatus;
	private Boolean sponsorsResearch;
	private Date involvementStartDate;
	private Date involvementEndDate;
	private String studentInvolvement;
	private String staffInvolvement;
	private String instituteResourceInvolvement;
	private Timestamp updateTimestamp;
	private String updateUser;
	private String createUser;
	private Timestamp createTimestamp;
	private String personFullName;
	private String revisionReason;

}
