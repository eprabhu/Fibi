package com.polus.fibicomp.businessrule.metarule.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.businessrule.metarule.vo.MetaRuleVO;

@Service
public interface MetaRuleService {

	/**
	 * This method is used for fetching meta rule
	 * @param metaRuleVO
	 * @return 
	 */
	public String fetchMetaRulesByParams(MetaRuleVO metaRuleVO);
	/**
	 * This method is used for saveOrUpdate meta rule
	 * @param metaRuleVO
	 * @return 
	 */
	public String saveOrUpdateMetaRule(MetaRuleVO metaRuleVO);

	/**
	 * This method is used for delete meta rule node
	 * @param metaRuleId 
	 * @param vo
	 * @return 
	 */
	public String deleteMetaRuleNode(MetaRuleVO metaRuleVO);

}
