package com.polus.integration.feedentity.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;

import com.polus.integration.feedentity.config.FeignClientConfig;
import com.polus.integration.feedentity.dto.EntityDTO;
import com.polus.integration.feedentity.dto.EntityResponse;

@FeignClient(name = "KC-CONNECT", url = "${kc.integration.client.url}", configuration = FeignClientConfig.class)
public interface KCFeignClient {

	@PostMapping("/entity/feedEntityDetails")
	public EntityResponse feedEntityDetails(EntityDTO entityDTOs);

}
