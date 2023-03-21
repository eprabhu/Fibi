package com.polus.fibicomp.inbox.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.inbox.vo.InboxVO;

@RestController
public class InboxController {

	protected static Logger logger = LogManager.getLogger(InboxController.class.getName());

	@Autowired
	@Qualifier(value = "inboxService")
	private InboxService inboxService;

	@RequestMapping(value = "/showInbox", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String showInbox(@RequestBody InboxVO vo, HttpServletRequest request, HttpServletResponse response) {
		return inboxService.showInbox(vo);
	}

	@RequestMapping(value = "/markAsReadInboxMessage", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String markAsReadInboxMessage(@RequestBody InboxVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		return inboxService.markReadMessage(vo);
	}

}
