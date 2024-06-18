package com.polus.fibicomp.coi.clients.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class FormRequest {
	
	private Integer formBuilderId;

	private String moduleItemCode;
	
	private String moduleSubItemCode;
	
	private String moduleItemKey;
	
	private String moduleSubItemKey;
	
	private String documentOwnerPersonId;	
	
}
