package com.polus.fibicomp.coi.dto;


import com.polus.core.pojo.Country;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import lombok.Getter;
import lombok.Setter;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

@Getter
@Setter
public class PersonEntityDto {

	private Integer personEntityId;
	private Integer personEntityNumber;
	private String personId;
	private Integer entityId;
	private Integer entityNumber;
	private Boolean isFormCompleted;
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
	private String updateUserFullName;
	private List<PersonEntityRelationship> personEntityRelationships;
	private Country country;
	private EntityType entityType;
	private String actionTypeCode;
	private String entityName;
	private String relationshipName;
}
