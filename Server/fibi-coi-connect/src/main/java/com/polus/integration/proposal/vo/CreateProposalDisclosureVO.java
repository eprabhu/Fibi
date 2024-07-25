package com.polus.integration.proposal.vo;


import com.polus.integration.proposal.dto.CoiDisclosureDTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateProposalDisclosureVO {

	private CoiDisclosureDTO coiDisclosure;

	private String personId;

}
