package com.polus.fibicomp.businessrule.metarule.service;

import java.util.ArrayList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.businessrule.metarule.dao.MetaRuleDao;
import com.polus.fibicomp.businessrule.metarule.dto.MetaRuleDto;
import com.polus.fibicomp.businessrule.metarule.pojo.MetaRule;
import com.polus.fibicomp.businessrule.metarule.pojo.MetaRuleDetail;
import com.polus.fibicomp.businessrule.metarule.vo.MetaRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;


@Transactional
@Service(value = "metaRuleService")
public class MetaRuleServiceImpl implements MetaRuleService {

	protected static Logger logger = LogManager.getLogger(MetaRuleServiceImpl.class.getName());

	@Autowired
	private MetaRuleDao metaRuleDao;
	
	@Autowired
	private CommonDao commonDao;

	@Override
	public String fetchMetaRulesByParams(MetaRuleVO metaRuleVO) {
		MetaRule metaRule = metaRuleDao.fetchMetaRulesByParams(metaRuleVO.getUnitNumber(), metaRuleVO.getModuleCode(), metaRuleVO.getSubModuleCode(), metaRuleVO.getMetaRuleType());
		prepareMetaRuleHierarchy(metaRule, metaRuleVO);
		return commonDao.convertObjectToJSON(metaRuleVO);
	}

	private MetaRuleVO prepareMetaRuleHierarchy(MetaRule metaRule, MetaRuleVO metaRuleVO) {
		List<MetaRuleDto> nodes = new ArrayList<>();
		if (metaRule != null) {
			List<MetaRuleDetail> metaRuleDetails = metaRuleDao.getMetaRuleDetailByMetaRuleId(metaRule.getMetaRuleId());
			prepareNodeDetail(nodes, metaRuleDetails, metaRule.getMetaRuleId());
			metaRuleVO.setMetaRuleDtos(nodes);
			metaRuleVO.setMetaRule(metaRule);
		}
		return metaRuleVO;
	}

	private List<MetaRuleDto> prepareNodeDetail(List<MetaRuleDto> nodes, List<MetaRuleDetail> metaRuleDetails, Integer metaRuleId) {
		MetaRuleDto metaRuleDto = new MetaRuleDto();
		metaRuleDetails.stream().filter(metaRuleDetail -> metaRuleDetail.getParentNode().equals(0)).forEach(metaRuleDetail -> {
			setMetaRuleDtoDetail(metaRuleDto, metaRuleId, metaRuleDetail);
			List<MetaRuleDto> childNodes = new ArrayList<>();
			prepareChildNodes(metaRuleDto, childNodes, metaRuleDetail, metaRuleId, metaRuleDetails);
			nodes.add(metaRuleDto);
		});
		return nodes;
	}

	private MetaRuleDto setMetaRuleDtoDetail(MetaRuleDto metaRuleDto, Integer metaRuleId, MetaRuleDetail metaRuleDetail) {
		metaRuleDto.setMetaRuleId(metaRuleId);
		metaRuleDto.setMetaRuleDetailId(metaRuleDetail.getMetaRuleDetailId());
		metaRuleDto.setRuleId(metaRuleDetail.getRuleId());
		metaRuleDto.setRuleName(metaRuleDao.getRuleNameByRuleId(metaRuleDetail.getRuleId()));
		metaRuleDto.setNodeNumber(metaRuleDetail.getNodeNumber());
		metaRuleDto.setParentNodeNumber(metaRuleDetail.getParentNode());
		metaRuleDto.setNextNode(metaRuleDetail.getNextNode());
		metaRuleDto.setNodeIfFalse(metaRuleDetail.getNodeIfFalse());
		metaRuleDto.setNodeIfTrue(metaRuleDetail.getNodeIfTrue());
		return metaRuleDto;
	}

	private MetaRuleDto prepareChildNodes(MetaRuleDto metaRuleDto, List<MetaRuleDto> childNodes, MetaRuleDetail metaRuleDetail, Integer metaRuleId, List<MetaRuleDetail> metaRuleDetails) {
		Integer nodeNumber = metaRuleDetail.getParentNode().equals(0) ? metaRuleDetail.getNodeNumber() : metaRuleDetail.getParentNode();
		List<MetaRuleDetail> childMetaRuleDetails = metaRuleDao.getMetaRuleDetailsByParams(nodeNumber, metaRuleId);
		childMetaRuleDetails.forEach(childMetaRuleDetail -> {
			MetaRuleDto childMetaRuleDto = new MetaRuleDto();
			metaRuleDetails.stream().filter(parentNode -> parentNode.getNodeNumber().equals(childMetaRuleDetail.getParentNode()))
			.forEach(parent -> {
				String nodeCondition = null;
				if (parent.getNodeIfFalse() != null && parent.getNodeIfFalse().equals(childMetaRuleDetail.getNodeNumber())) {
					nodeCondition = Constants.NODE_IF_FALSE;
				} else if (parent.getNodeIfTrue() != null && parent.getNodeIfTrue().equals(childMetaRuleDetail.getNodeNumber())) {
					nodeCondition = Constants.NODE_IF_TRUE;
				} else if (parent.getNextNode() != null && parent.getNextNode().equals(childMetaRuleDetail.getNodeNumber())) {
					nodeCondition = Constants.NEXT_NODE;
				}
				childMetaRuleDto.setNodeCondition(nodeCondition);
			});
			setMetaRuleDtoDetail(childMetaRuleDto, metaRuleId, childMetaRuleDetail);
			childNodes.add(childMetaRuleDto);
			prepareInnerChildNodes(childMetaRuleDto, childMetaRuleDetail.getNodeNumber(), metaRuleId, metaRuleDetails);
		});
		metaRuleDto.setChildNodes(childNodes);
		return metaRuleDto;
	}

	private MetaRuleDto prepareInnerChildNodes(MetaRuleDto childMetaRuleDto, Integer childMetaRuleDetailNodeNumber, Integer metaRuleId, List<MetaRuleDetail> metaRuleDetails) {
		List<MetaRuleDetail> innerChildMetaRuleDetails = metaRuleDao.getMetaRuleDetailsByParams(childMetaRuleDetailNodeNumber, metaRuleId);
		if (innerChildMetaRuleDetails != null && !innerChildMetaRuleDetails.isEmpty()) {
			innerChildMetaRuleDetails.forEach(innerChildMetaRuleDetail -> {
				List<MetaRuleDto> innerChildNodes = new ArrayList<>();
				prepareChildNodes(childMetaRuleDto, innerChildNodes, innerChildMetaRuleDetail, metaRuleId, metaRuleDetails);
				childMetaRuleDto.setChildNodes(innerChildNodes);
			});
		}
		return childMetaRuleDto;
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateMetaRule(MetaRuleVO metaRuleVO) {
		try {
			MetaRule metaRule = metaRuleVO.getMetaRule();
			List<MetaRuleDetail> metaRuleDetails = metaRule.getMetaRuleDetails();
			Integer nodeNumber = null;
			for(MetaRuleDetail metaRuleDetail : metaRuleDetails) {
				if (metaRuleDetail.getMetaRuleDetailId() == null) {
					nodeNumber = metaRuleDao.generateNextNodeNumber(metaRuleVO.getMetaRule().getMetaRuleId());
					metaRuleDetail.setNodeNumber(nodeNumber);
				}
				if (Boolean.FALSE.equals(metaRuleVO.getMetaRuleAvailable())) {
					metaRuleDetail.setParentNode(0);
				}
				metaRuleDetail.setMetaRule(metaRule);
			}
			metaRuleDao.saveOrUpdateMetaRule(metaRule);
			if (metaRuleVO.getParentNodeNumber() != null) {
				metaRuleDao.updateParentNodeDetails(nodeNumber, metaRuleVO.getParentNodeNumber(), metaRule.getMetaRuleId(), metaRuleVO.getNodeCondition());
			}
			prepareMetaRuleHierarchy(metaRule, metaRuleVO);
		} catch (Exception e) {
			logger.error(" error occured in saveOrUpdateMetaRule: {}", e.getMessage());
			throw new ApplicationException("Error occured while saveOrUpdateMetaRule", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(metaRuleVO);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String deleteMetaRuleNode(MetaRuleVO metaRuleVO) {
		try {
			metaRuleDao.deleteMetaRuleNode(metaRuleVO.getMetaRuleDetailId());
			if (Boolean.TRUE.equals(metaRuleVO.getIsRootNode())) {
				metaRuleDao.deleteMetaRule(metaRuleVO.getMetaRuleId());
			} else {
				metaRuleDao.updateNodeDetailInParent(metaRuleVO.getParentNodeNumber(), metaRuleVO.getMetaRuleId(), metaRuleVO.getNodeCondition());
			}
		} catch (Exception e) {
			logger.error(" error occured in saveOrUpdateMetaRule: {}", e.getMessage());
			throw new ApplicationException("Error occured while saveOrUpdateMetaRule", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(metaRuleVO);
	}

}
