package com.polus.fibicomp.globalentity.dto;

import java.util.List;

import com.polus.fibicomp.globalentity.pojo.EntityAttachment;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ComplianceResponseDTO {

	private List<EntityRisk> entityRisks;
	private List<EntityAttachment> attachments;

}
