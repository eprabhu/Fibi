package com.polus.fibicomp.globalentity.dashboard.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EntityDashboardResponse {

	private Integer entityId;
	private Integer entityNumber;
	private String entityName;
	private String ownershipType;
	private String primaryAddressLine1;
	private String primaryAddressLine2;
	private String country;
	private String city;
	private String state;
	private String dunsNumber;
	private String ueiNumber;
	private String cageNumber;
	private String websiteAddress;
	private String certifiedEmail;
	private String entityStatus;
	private String entityVerificationStatus;
	private String entityStatusTypeCode;
	private String documentStatusTypeCode;
	private String ownershipTypeCode;

}
