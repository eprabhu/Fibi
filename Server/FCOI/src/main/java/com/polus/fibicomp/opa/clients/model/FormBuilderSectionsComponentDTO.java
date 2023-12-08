package com.polus.fibicomp.opa.clients.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FormBuilderSectionsComponentDTO {

	private Integer componentId;
	
	private Integer sectionId;
	
	private String componentDescription;
	
	private String componentType;
	
	private Integer ComponentOrder;
	
	private String componentRefId;
	
	private String componentData;
	
	private String componentHeader;
	
	private String componentFooter;
}