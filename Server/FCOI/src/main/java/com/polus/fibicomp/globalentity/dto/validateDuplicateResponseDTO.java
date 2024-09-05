package com.polus.fibicomp.globalentity.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class validateDuplicateResponseDTO {

	private Integer entityId;
	private String entityName;
	private String primaryAddressLine2;
	private String primaryAddressLine1;
	private String country;
	private String city;
	private String state;
	private String dunsNumber;
	private String ueiNumber;
	private String cageNumber;
	private String website;
	private String email;
	private String sponsorCode;
	private Integer organizationId;

}
