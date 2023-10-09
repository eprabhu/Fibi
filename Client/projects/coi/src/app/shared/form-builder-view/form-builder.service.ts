import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { FormBuilderSaveRO } from './form-builder-interface';

@Injectable()

export class FormBuilderService {

    // tslint:disable:max-line-length
    DUMMY = {
        'applicableFormsBuilderIds': [
            2
        ],
        'form': {
            'formBuilderId': 2,
            'formBuilderNumber': '1002',
            'moduleItemCode': '23',
            'moduleSubItemCode': '0',
            'moduleItemKey': '23111111',
            'moduleSubItemKey': '0',
            'formName': 'Staff OPA Form <--> ACTION TIME :  25012',
            'formSections': [
                {
                    'sectionId': 113,
                    'sectionName': 'STAFF OUTSIDE PROFESSIONAL ACTIVITIES',
                    'sectionOrder': 1,
                    'sectionDescription': 'STAFF OUTSIDE PROFESSIONAL ACTIVITIES',
                    'sectionBusinessRule': null,
                    'sectionHelpText': 'Help Text for staff OPA',
                    'sectionHeader': 'Based on your job category as an administrative staff, you are not required to submit an OPA report unless: 1) You have been given approval by your department head/supervisor to undertake a specific instance of professional consulting service, or 2) You have engaged in professional consulting service for which you still need to seek department head/supervisor approval For the purposes of OPA professional consulting normally is defined as outside activity that is related to your professional role at MIT. It does not include activities that are unrelated to your administrative role​ or responsibilities at MIT',
                    'sectionFooter': 'Please note that you must keep your \'supervisor informed of any outside activities, professional or otherwise; that have the potential to diminish your ability to perform you normal job responsibilities and maintain your professional contribution to MIT If you are the supervisor of an employee with outside professional activities, please keep your department\'s OPA administrator award of the activities.',
                    'sectionComponent': [
                        {
                            'componentId': 5,
                            'sectionId': 113,
                            'componentDescription': null,
                            'componentType': 'CE',
                            'componentRefId': '114',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': {
                                'customDataElement': null,
                                'customDataElements': null,
                                'responseMessage': null,
                                'customDataElementId': null,
                                'customDataTypes': null,
                                'elementOptions': null,
                                'customResponses': null,
                                'moduleCode': null,
                                'customElements': [
                                    {
                                        'customDataElementId': 114,
                                        'columnName': 'Did you engage in Professionals consultation during the Report Period',
                                        'defaultValue': '',
                                        'dataType': '4',
                                        'isRequired': 'N',
                                        'options': [
                                            {
                                                'optionName': 'Yes',
                                                'customDataOptionId': '19'
                                            },
                                            {
                                                'optionName': 'No',
                                                'customDataOptionId': '20'
                                            }
                                        ],
                                        'answers': [],
                                        'moduleItemCode': 23,
                                        'moduleItemKey': '23111111',
                                        'subModuleItemCode': 0,
                                        'subModuleItemKey': '0',
                                        'dataLength': null,
                                        'columnId': 171,
                                        'versionNumber': 1,
                                        'lookupWindow': '',
                                        'lookupArgument': '',
                                        'filterType': 'Check Box',
                                        'orderNumber': 1,
                                        'isActive': null,
                                        'customElementName': null
                                    }
                                ],
                                'updateUser': null,
                                'updateTimestamp': null,
                                'moduleItemKey': null,
                                'applicableModules': null,
                                'deleteOptions': null,
                                'isDataChange': null,
                                'dataTypeCode': null,
                                'systemLookups': null,
                                'lookUps': null,
                                'elementAnswered': null,
                                'subModuleCode': null,
                                'subModuleItemKey': null
                            },
                            'componentOrder': 1
                        }
                    ]
                },
                {
                    'sectionId': 114,
                    'sectionName': 'STAFF DATA RECORD',
                    'sectionOrder': 2,
                    'sectionDescription': 'STAFF DATA RECORD',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 6,
                            'sectionId': 114,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1100',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1100,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1069,
                                    'QUESTIONNAIRE_ID': 1100,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- Staff Data Record',
                                    'UPDATE_TIMESTAMP': '2023-10-04T11:25:02.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 925,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'The checkbox indicates from the Institute records if your appointment is full-time or part-time.',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2154,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Checkbox',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        },
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 926,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'Appointment percentage',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 2,
                                            'QUESTION_ID': 2155,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Text',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        },
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 927,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'The checkbox indicates from the Institute records if you have compensated appointment.',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 3,
                                            'QUESTION_ID': 2156,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Y/N',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3361,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2154,
                                            'OPTION_LABEL': 'Full-Time',
                                            'REQUIRE_EXPLANATION': null
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3362,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2154,
                                            'OPTION_LABEL': 'Part-Time',
                                            'REQUIRE_EXPLANATION': null
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3363,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2156,
                                            'OPTION_LABEL': 'Yes',
                                            'REQUIRE_EXPLANATION': null
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3364,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2156,
                                            'OPTION_LABEL': 'No',
                                            'REQUIRE_EXPLANATION': null
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 1
                        },
                        {
                            'componentId': 7,
                            'sectionId': 114,
                            'componentDescription': null,
                            'componentType': 'HL',
                            'componentRefId': null,
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': null,
                            'componentOrder': 2
                        }
                    ]
                },
                {
                    'sectionId': 115,
                    'sectionName': 'STAFF CONSULTING',
                    'sectionOrder': 3,
                    'sectionDescription': 'STAFF CONSULTING',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': 'Note. With the exception of Senior Research; Scientists/Engineers/Associates and Principal Research Scientists most MIT Staff do not have consulting privileges',
                    'sectionFooter': '<p><span style=\\"color:#52A5E6;\\">If </span><span style=\\"color:#0054A5;\\">yes, </span><span style=\\"color:#52A5E6;\\">please </span><span style=\\"color:#0054A5;\\">complete the </span><span style=\\"color:#52A5E6;\\">next </span><span style=\\"color:#83C6FF;\\">section, </span>Compensated <span style=\\"color:#0082C6;\\">and </span><span style=\\"color:#000083;\\">Uncompensated </span><span style=\\"color:#83C6FF;\\">Professional </span>Activities. </p><p><span style=\\"color:#52A5E6;\\">If </span><span style=\\"color:#838200;\\">no, </span><span style=\\"color:#0054A5;\\">you </span><span style=\\"color:#A5A583;\\">may </span>skip <span style=\\"color:#0054A5;\\">the </span><span style=\\"color:#838200;\\">next </span><span style=\\"color:#0054A5;\\">section</span></p>',
                    'sectionComponent': [
                        {
                            'componentId': 8,
                            'sectionId': 115,
                            'componentDescription': null,
                            'componentType': 'RT',
                            'componentRefId': null,
                            'componentData': '<p>During the past year, did you work or consult, either paid or unpaid, for any organization or entity outside <span style=\\"color:hsl(0, 75%, 60%);\\"><strong>MIT</strong></span> in a professional capacity, i.e., performing work that was related to your professional role at <span style=\\"color:hsl(0, 75%, 60%);\\"><strong>MIT</strong></span>* (Please note that you will have an opportunity to report outside activities unrelated to your professional role in the \\"Other Activities\\" section, below.)<br> </p>',
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': null,
                            'componentOrder': 1
                        },
                        {
                            'componentId': 9,
                            'sectionId': 115,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1101',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1101,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1070,
                                    'QUESTIONNAIRE_ID': 1101,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- Staff Consulting',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:10:10.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 928,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'During the past year, did you work or consult, either paid or unpaid, for any organization or entity outside MIT in a professional cap a city, i.e., performing work that was related to your professional role at MIT* (Please note that you will have an opportunity to report outside activities\' unrelated to your professional role in the "Other Activities" section, below.)',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2157,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Y/N',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3365,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2157,
                                            'OPTION_LABEL': 'Yes',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3366,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2157,
                                            'OPTION_LABEL': 'No',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 2
                        }
                    ]
                },
                {
                    'sectionId': 116,
                    'sectionName': 'COMPENSATED & UNCOMPENSATED ACTIVITIES',
                    'sectionOrder': 4,
                    'sectionDescription': 'COMPENSATED & UNCOMPENSATED ACTIVITIES',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': '',
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 10,
                            'sectionId': 116,
                            'componentDescription': null,
                            'componentType': 'RT',
                            'componentRefId': null,
                            'componentData': '<p><span style=\\"color:hsl(0, 0%, 0%);\\">List all compensated outside professional activities and all uncompensated outside professional activities i.e. requiring substantial time commitment with no, or nominal compensation.</span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\"><strong>** Denotes entities you have sync\'d from the COI module. </strong></span></p><p> </p><p><span style=\\"color:hsl(0, 0%, 0%);\\">List all compensated outside professional activities, both domestic and international; and all uncompensated outside professional activities (i.e., requiring substantial time commitment with no or nominal compensation), both domestic and international. </span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">Examples of c<strong><u>ompensated outside professional activities</u></strong>:</span></p><p> </p><p><span style=\\"color:hsl(0, 0%, 0%);\\">* Consulting or research for a company, university or other entity, including service as an expert witness</span></p><p><span style=\\"color:hsl(0',
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': null,
                            'componentOrder': 1
                        },
                        {
                            'componentId': 11,
                            'sectionId': 116,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1102',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1102,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1071,
                                    'QUESTIONNAIRE_ID': 1102,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- Compensated Uncompensated',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:14:30.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 929,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'List all compensated outside professional activities.',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2158,
                                            'ANSWER_TYPE': 'Table',
                                            'NO_OF_ANSWERS': null,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3367,
                                            'OPTION_NUMBER': 1,
                                            'QUESTION_ID': 2158,
                                            'OPTION_LABEL': 'Company/Entity',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3368,
                                            'OPTION_NUMBER': 2,
                                            'QUESTION_ID': 2158,
                                            'OPTION_LABEL': 'Is Location outside US?',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3369,
                                            'OPTION_NUMBER': 3,
                                            'QUESTION_ID': 2158,
                                            'OPTION_LABEL': 'Nature of Work/Relationship',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3370,
                                            'OPTION_NUMBER': 4,
                                            'QUESTION_ID': 2158,
                                            'OPTION_LABEL': 'Compensated or Uncompensated?',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3371,
                                            'OPTION_NUMBER': 5,
                                            'QUESTION_ID': 2158,
                                            'OPTION_LABEL': '# of Days per Year',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 2
                        }
                    ]
                },
                {
                    'sectionId': 117,
                    'sectionName': 'OTHER ACTIVITIES',
                    'sectionOrder': 5,
                    'sectionDescription': 'OTHER ACTIVITIES',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': 'During the past year did you engage in any outside \\\'/ork or activity. paid or unpaid of a non-professional nature i.e., activity that was unrelated to your professional role at MIT, that in any way diminished your ability to fulfill your professional responsibility to MIT?',
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 12,
                            'sectionId': 117,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1103',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1103,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1072,
                                    'QUESTIONNAIRE_ID': 1103,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- Other Activities',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:26:48.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 930,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': 'Y',
                                            'QUESTION': 'During the past year did you engage in any outside work or activity. paid or unpaid of a non-professional nature i.e., activity that was unrelated to your professional role at MIT, that in any way diminished your ability to fulfill your professional responsibility to MIT?',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2159,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Y/N',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        },
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 931,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'If yes, please describe the activity and indicate whether you have discussed this Activity with your supervisor',
                                            'GROUP_NAME': 'G1',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 2,
                                            'QUESTION_ID': 2160,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Textarea',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': 2159
                                        }
                                    ],
                                    'conditions': [
                                        {
                                            'QUESTION_CONDITION_ID': 1376,
                                            'CONDITION_TYPE': 'EQUALS',
                                            'GROUP_NAME': 'G1',
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_ID': 2159,
                                            'CONDITION_VALUE': 'Yes'
                                        }
                                    ],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3372,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2159,
                                            'OPTION_LABEL': 'Yes',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3373,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2159,
                                            'OPTION_LABEL': 'No',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 1
                        }
                    ]
                },
                {
                    'sectionId': 118,
                    'sectionName': 'OUTSIDE FINANCIAL INTERESTS AND RELATIONSHIPS',
                    'sectionOrder': 6,
                    'sectionDescription': 'OUTSIDE FINANCIAL INTERESTS AND RELATIONSHIPS',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 13,
                            'sectionId': 118,
                            'componentDescription': null,
                            'componentType': 'RT',
                            'componentRefId': null,
                            'componentData': '<p><span style=\\"color:hsl(0, 0%, 0%);\\">For the current reporting period, list any company or other entity, both domestic and international that has a relationship with your MIT activities in any way and from which you, or any member of your immediate family, received any payments (including honoraria or royalties) for employment, consulting, board membership, or other relationship, or in which you have an ownership or equity interest. </span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">             </span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">Example of <strong><u>relationships to MIT activities that an outside entity could have</u></strong>:</span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">* The company/entity sponsors research or teaching activities at MIT in which you are directly involved</span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">* The company/entity has made gifts to MIT which are under your control or directly benefit your MIT research or teaching activities</',
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': null,
                            'componentOrder': 1
                        },
                        {
                            'componentId': 14,
                            'sectionId': 118,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1104',
                            'componentData': null,
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1104,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1073,
                                    'QUESTIONNAIRE_ID': 1104,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA-Outside Financial Interest',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:50:15.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 932,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'List any company or other entity',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2161,
                                            'ANSWER_TYPE': 'Table',
                                            'NO_OF_ANSWERS': null,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3374,
                                            'OPTION_NUMBER': 1,
                                            'QUESTION_ID': 2161,
                                            'OPTION_LABEL': 'Company/Entity',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3375,
                                            'OPTION_NUMBER': 2,
                                            'QUESTION_ID': 2161,
                                            'OPTION_LABEL': 'Located Outside US',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3376,
                                            'OPTION_NUMBER': 3,
                                            'QUESTION_ID': 2161,
                                            'OPTION_LABEL': 'Relationship',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3377,
                                            'OPTION_NUMBER': 4,
                                            'QUESTION_ID': 2161,
                                            'OPTION_LABEL': 'Your Relationship with Company/Entity',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3378,
                                            'OPTION_NUMBER': 5,
                                            'QUESTION_ID': 2161,
                                            'OPTION_LABEL': 'Company\'s/Entity\'s Relationship with MIT',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 2
                        }
                    ]
                },
                {
                    'sectionId': 119,
                    'sectionName': 'INVOLVEMENT OF STUDENTS OR SUBORDINATE EMPLOYEES',
                    'sectionOrder': 7,
                    'sectionDescription': 'INVOLVEMENT OF STUDENTS OR SUBORDINATE EMPLOYEES',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 15,
                            'sectionId': 119,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1106',
                            'componentData': null,
                            'componentHeader': '<p>List the names of any MIT students or subordinate employees (paid or unpaid) who have been involved in any of your outside professional activities, including companies with which you have a consulting, board membership, ownership or other relationship, and describe the nature of their work, the amount of time involved, and your relationship with each student (e.g., thesis supervisor, supervisor of the student as an RA or TA, etc.) or subordinate (e.g., post-doc, support staff, etc.) </br> </p>',
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1106,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1075,
                                    'QUESTIONNAIRE_ID': 1106,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- INVOLVEMENT OF STUDENTS OR SUBORDINATE EMPLOYEES',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:57:36.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 934,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'List the names',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2163,
                                            'ANSWER_TYPE': 'Table',
                                            'NO_OF_ANSWERS': null,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3381,
                                            'OPTION_NUMBER': 1,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': 'Student / Subordinate',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3382,
                                            'OPTION_NUMBER': 2,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': 'Name',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3383,
                                            'OPTION_NUMBER': 3,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': 'Describe the type of Work',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3384,
                                            'OPTION_NUMBER': 4,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': 'Company',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3385,
                                            'OPTION_NUMBER': 5,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': 'Relationship with student/subordinate',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3386,
                                            'OPTION_NUMBER': 6,
                                            'QUESTION_ID': 2163,
                                            'OPTION_LABEL': '# of Days',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 1
                        }
                    ]
                },
                {
                    'sectionId': 120,
                    'sectionName': 'USE OF INSTITUTE RESOURCES',
                    'sectionOrder': 8,
                    'sectionDescription': 'USE OF INSTITUTE RESOURCES',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 16,
                            'sectionId': 120,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1105',
                            'componentData': null,
                            'componentHeader': '<p>MIT resources (facilities, equipment, funds) should be used only for Institute purposes, except when other purposes have been given prior approval by an appropriate MIT senior officer.<br> </p>',
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1105,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1074,
                                    'QUESTIONNAIRE_ID': 1105,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA- USE OF INSTITUTE RESOURCES',
                                    'UPDATE_TIMESTAMP': '2023-10-04T12:54:09.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 933,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'List all the institute Resources',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2162,
                                            'ANSWER_TYPE': 'Table',
                                            'NO_OF_ANSWERS': null,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3379,
                                            'OPTION_NUMBER': 1,
                                            'QUESTION_ID': 2162,
                                            'OPTION_LABEL': 'Company/Entity',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3380,
                                            'OPTION_NUMBER': 2,
                                            'QUESTION_ID': 2162,
                                            'OPTION_LABEL': 'Please describe the use of MIT resources',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 1
                        }
                    ]
                },
                {
                    'sectionId': 121,
                    'sectionName': 'POTENTIAL CONFLICT OF INTEREST OR COMMITMENT',
                    'sectionOrder': 9,
                    'sectionDescription': 'POTENTIAL CONFLICT OF INTEREST OR COMMITMENT',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 17,
                            'sectionId': 121,
                            'componentDescription': null,
                            'componentType': 'QN',
                            'componentRefId': '1107',
                            'componentData': null,
                            'componentHeader': '<p>It is the policy of the Institute that its faculty and staff have the obligation to avoid financial or other conflicts of interest and to ensure that their activities and interests do not conflict with their obligations to the Institute or its welfare. In view of the Institute\'s policy on conflict of interest and commitment, do any of the activities or relationships described in this report have the potential for, or the appearance of, a conflict of interest or commitment?</br></p>',
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': {
                                'applicableQuestionnaire': null,
                                'questionnaireId': 1107,
                                'moduleItemKey': '23111111',
                                'moduleSubItemKey': '0',
                                'moduleItemCode': 23,
                                'moduleSubItemCode': 0,
                                'questionnaireAnswerHeaderId': null,
                                'questionnaireAnsAttachmentId': null,
                                'questionnaireCompleteFlag': null,
                                'actionUserId': null,
                                'actionPersonId': null,
                                'actionPersonName': null,
                                'acType': null,
                                'questionnaireName': null,
                                'newQuestionnaireVersion': false,
                                'questionEditted': false,
                                'questionnaireList': null,
                                'questionnaireGroup': null,
                                'header': {
                                    'TRIGGER_POST_EVALUATION': null,
                                    'QUESTIONNAIRE_VERSION': 1,
                                    'UPDATE_USER': 'willsmith',
                                    'ANS_UPDATE_TIMESTAMP': null,
                                    'ANS_PERSON_FULL_NAME': null,
                                    'QUESTIONNAIRE_DESCRIPTION': null,
                                    'IS_FINAL': true,
                                    'QUESTIONNAIRE_NUMBER': 1076,
                                    'QUESTIONNAIRE_ID': 1107,
                                    'QUEST_GROUP_TYPE_CODE': null,
                                    'AC_TYPE': 'U',
                                    'QUESTIONNAIRE_NAME': 'OPA - POTENTIAL CONFLICT OF INTEREST OR COMMITMENT',
                                    'UPDATE_TIMESTAMP': '2023-10-04T13:01:14.000+00:00'
                                },
                                'questionnaire': {
                                    'maxGroupNumber': null,
                                    'questions': [
                                        {
                                            'LOOKUP_TYPE': null,
                                            'LOOKUP_FIELD': null,
                                            'ANSWER_LENGTH': null,
                                            'RULE_ID': null,
                                            'GROUP_LABEL': null,
                                            'UPDATE_USER': 'willsmith',
                                            'QUESTION_NUMBER': 935,
                                            'LOOKUP_NAME': null,
                                            'HAS_CONDITION': null,
                                            'QUESTION': 'It is the policy of the Institute that its faculty and staff have the obligation to avoid financial or other conflicts of interest and to ensure that their activities and interests do not conflict with their obligations to the Institute or its welfare. In view of the Institute\'s policy on conflict of interest and commitment, do any of the activities or relationships described in this report have the potential for, or the appearance of, a conflict of interest or commitment?',
                                            'SHOW_QUESTION': true,
                                            'GROUP_NAME': 'G0',
                                            'HELP_LINK': null,
                                            'QUESTION_VERSION_NUMBER': 1,
                                            'DESCRIPTION': null,
                                            'SORT_ORDER': 1,
                                            'QUESTION_ID': 2164,
                                            'ANSWERS': {
                                                '1': ''
                                            },
                                            'ANSWER_TYPE': 'Y/N',
                                            'NO_OF_ANSWERS': 1,
                                            'UPDATE_TIMESTAMP': '2023-10-03T23:00:00.000+00:00',
                                            'PARENT_QUESTION_ID': null
                                        }
                                    ],
                                    'conditions': [],
                                    'options': [
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3387,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2164,
                                            'OPTION_LABEL': 'Yes',
                                            'REQUIRE_EXPLANATION': 'N'
                                        },
                                        {
                                            'EXPLANTION_LABEL': null,
                                            'QUESTION_OPTION_ID': 3388,
                                            'OPTION_NUMBER': null,
                                            'QUESTION_ID': 2164,
                                            'OPTION_LABEL': 'No',
                                            'REQUIRE_EXPLANATION': 'N'
                                        }
                                    ],
                                    'deleteList': null,
                                    'questionnaireQuestions': null,
                                    'questionnaireConditions': null,
                                    'questionnaireOptions': null,
                                    'questionnaireAnswers': null,
                                    'quesAttachmentList': null,
                                    'lookUpDetails': null
                                },
                                'usage': null,
                                'fileName': null,
                                'fileContent': null,
                                'length': null,
                                'remaining': null,
                                'fileTimestamp': null,
                                'contentType': null,
                                'personId': null,
                                'multipartFile': null,
                                'moduleList': null,
                                'isInserted': null,
                                'updateTimestamp': null,
                                'copyModuleItemKey': null,
                                'questionnaireNumbers': null,
                                'lookUpDetails': null,
                                'newQuestionnaireId': null,
                                'moduleSubItemCodes': [],
                                'questionnaireBusinessRules': null,
                                'ruleId': null,
                                'rulePassed': null,
                                'questionnaireMode': null,
                                'copyInActiveQuestionAnswers': false
                            },
                            'customElement': null,
                            'componentOrder': 1
                        }
                    ]
                },
                {
                    'sectionId': 122,
                    'sectionName': 'CERTIFICATION',
                    'sectionOrder': 10,
                    'sectionDescription': 'CERTIFICATION',
                    'sectionBusinessRule': null,
                    'sectionHelpText': null,
                    'sectionHeader': null,
                    'sectionFooter': null,
                    'sectionComponent': [
                        {
                            'componentId': 18,
                            'sectionId': 122,
                            'componentDescription': null,
                            'componentType': 'RT',
                            'componentRefId': null,
                            'componentData': '<p><span style=\\"color:hsl(0,0%,0%);\\">I agree to abide by MIT\'s policies on Full-time Service, Conflict of Interest and Outside Professional Activities (as stated in MIT Policies and Procedures section 4.4).</span></p><p><a target=\\"_blank\\" rel=\\"noopener noreferrer\\" href=\\"https://web.mit.edu/policies/4/4.3.html\\"><span style=\\"color:hsl(150, 75%, 60%);\\">Faculty Rights and Responsibilities: Full Time Service</span></a></p><p><a target=\\"_blank\\" rel=\\"noopener noreferrer\\" href=\\"https://web.mit.edu/policies/4/4.4.html\\"><span style=\\"color:hsl(150, 75%, 60%);\\">Faculty Rights and Responsibilities: Conflict of Interest</span></a></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">I supply this information for confidential review by my department head and other officials designated by MIT\'s administration.</span></p><p><span style=\\"color:hsl(0, 0%, 0%);\\">I understand that this information may not be released by MIT except for limited purposes required by law, regulation or contract.</span',
                            'componentHeader': null,
                            'componentFooter': null,
                            'programmedElement': null,
                            'questionnaire': null,
                            'customElement': null,
                            'componentOrder': 1
                        }
                    ]
                }
            ]
        }
    };

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getFormBuilderData() {
        return of(this.DUMMY);
        // return this._http.post(this._commonService.baseUrl + '/formbuilder/getBlankForm', {
        //     'moduleItemCode': '23',
        //     'moduleSubItemCode': '0'
        // });
    }

    saveFormComponent(data: FormBuilderSaveRO): Observable<any> {
        const formData = new FormData();
        formData.append('formId', data.formId.toString());
        formData.append('documentOwnerPersonId', data.documentOwnerPersonId);
        formData.append('moduleItemCode', data.moduleItemCode);
        formData.append('moduleSubItemCode', data.moduleSubItemCode);
        formData.append('moduleItemKey', data.moduleItemKey);
        formData.append('moduleSubItemKey', data.moduleSubItemKey);
        formData.append('componentId', data.componentId.toString());
        formData.append('componentType', data.componentType);
        formData.append('programmedElement', JSON.stringify(data.programmedElement));
        formData.append('questionnaire', JSON.stringify(data.questionnaire));
        formData.append('customElement', JSON.stringify(data.customElement));
        if (data?.files?.length > 0) {
            data.files.forEach(file => {
                formData.append(file.questionId + '', file.attachment, file.attachment.name);
            });
        }
        return this._http.post(this._commonService.baseUrl + '/saveFormComponent', formData);
    }

}

