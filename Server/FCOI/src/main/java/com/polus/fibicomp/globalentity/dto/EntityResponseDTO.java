package com.polus.fibicomp.globalentity.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.polus.fibicomp.globalentity.pojo.EntityExternalIdMapping;
import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;
import com.polus.fibicomp.globalentity.pojo.EntityMailingAddress;
import com.polus.fibicomp.globalentity.pojo.EntityRegistration;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.Entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class EntityResponseDTO {

	private Entity entityDetails;
	private List<EntityIndustryClassification> entityIndustryClassifications;
	private List<EntityRegistration> entityRegistrations;
	private List<EntityMailingAddress> entityMailingAddresses;
	private List<EntityRisk> entityRisks;
	private List<EntityExternalIdMapping> entityExternalIdMappings;
	private List<PriorNameResponseDTO> priorNames;
	private List<ForeignNameResponseDTO> foreignNames;

}