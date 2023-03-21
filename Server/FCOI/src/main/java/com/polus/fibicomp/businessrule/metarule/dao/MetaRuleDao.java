package com.polus.fibicomp.businessrule.metarule.dao;

import java.util.List;

import com.polus.fibicomp.businessrule.metarule.pojo.MetaRule;
import com.polus.fibicomp.businessrule.metarule.pojo.MetaRuleDetail;

public interface MetaRuleDao {

	/**
	 * this method is used to save or update a Meta Rule
	 * @param metaRule
	 * @param acType
	 * @return
	 */
	public MetaRule saveOrUpdateMetaRule(MetaRule metaRule);

	/**
	 * this method is used to generate Next NodeNumber
	 * @param metaRuleId
	 * @return
	 */
	public Integer generateNextNodeNumber(Integer metaRuleId);

	/**
	 * this method is used to fetch MetaRules By params
	 * @param unitNumber
	 * @param moduleCode
	 * @param subModuleCode
	 * @param metaRuleType
	 * @return
	 */
	public MetaRule fetchMetaRulesByParams(String unitNumber, Integer moduleCode, Integer subModuleCode,
			String metaRuleType);

	/**
	 * @param parentNode
	 * @return
	 */
	public List<MetaRuleDetail> getMetaRuleDetailByMetaRuleId(Integer parentNode);

	/**
	 * @param metaRuleDetail
	 * @return
	 */
	public MetaRuleDetail saveOrUpdateMetaRuleDetail(MetaRuleDetail metaRuleDetail);

	/**
	 * this method is used to get Rule Name By RuleId
	 * @param ruleId
	 * @return
	 */
	public String getRuleNameByRuleId(Integer ruleId);

	/**
	 * this method is used to get MetaRuleDetails By nodeNumber and metaRuleId
	 * @param nodeNumber
	 * @param metaRuleId
	 * @return
	 */
	public List<MetaRuleDetail> getMetaRuleDetailsByParams(Integer nodeNumber, Integer metaRuleId);

	/**
	 * this method is used to delete a meta Rule node
	 * @param metaRuleDetailId
	 * @return
	 */
	public void deleteMetaRuleNode(Integer metaRuleDetailId);

	/**
	 * this method is used to delete a meta Rule
	 * @param metaRuleId
	 * @return
	 */
	public void deleteMetaRule(Integer metaRuleId);

	/**
	 * this method is used to delete a meta Rule
	 * @param nodeNumber
	 * @param parentNodeNumber
	 * @param metaRuleId
	 * @param nodeCondition
	 */
	public void updateParentNodeDetails(Integer nodeNumber, String parentNodeNumber, Integer metaRuleId, String nodeCondition);

	/**
	 * @param parentNodeNumber
	 * @param metaRuleDetailId
	 * @param nodeCondition
	 */
	public void updateNodeDetailInParent(String parentNodeNumber, Integer metaRuleDetailId, String nodeCondition);

}
