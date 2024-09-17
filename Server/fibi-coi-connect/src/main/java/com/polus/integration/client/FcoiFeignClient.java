package com.polus.integration.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.polus.integration.instituteProposal.vo.DisclosureSyncVO;
import com.polus.integration.proposal.vo.CreateProposalDisclosureVO;
import com.polus.integration.proposal.vo.MarkVoidVO;
import com.polus.integration.proposal.vo.ValidateDisclosureVO;
import com.polus.integration.vo.COIActionLogVO;

@FeignClient("FCOI")
public interface FcoiFeignClient {

	@PostMapping("/coi/fcoiDisclosure/validate")
	public ResponseEntity<Object> validateDisclosure(@RequestBody ValidateDisclosureVO vo);

	@PostMapping("/coi/fcoiDisclosure")
	public ResponseEntity<Object> createDisclosure(@RequestBody CreateProposalDisclosureVO vo);

	@PutMapping("/coi/fcoiDisclosure/integration/syncNeeded")
	public ResponseEntity<Object> updateProjectDisclosureFlag(@RequestBody DisclosureSyncVO vo);
	
	@PostMapping("/coi/fcoiDisclosure/integration/makeVoid")
	public ResponseEntity<Object> makeDisclosureVoid(@RequestBody MarkVoidVO vo);
	
	@PostMapping(value = "/coi/entity/logAction")
	public ResponseEntity<Object> logAction(@RequestBody COIActionLogVO vo);
	
}
