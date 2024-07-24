package com.polus.integration.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import com.polus.integration.proposal.vo.CreateProposalDisclosureVO;
import com.polus.integration.proposal.vo.ValidateDisclosureVO;

@FeignClient("FCOI")
public interface FcoiFeignClient {

	@PostMapping("/coi/validateDisclosure")
	public ResponseEntity<Object> validateDisclosure(@RequestBody ValidateDisclosureVO vo);

	@PostMapping("/coi/createDisclosure")
	public ResponseEntity<Object> createDisclosure(@RequestBody CreateProposalDisclosureVO vo);

}
