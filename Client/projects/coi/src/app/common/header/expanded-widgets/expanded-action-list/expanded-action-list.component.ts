import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { DATE_PLACEHOLDER } from 'projects/fibi/src/app/app-constants';
import { Constants } from 'projects/fibi/src/app/common/constants/action-list.constants';
import { CommonService } from '../../../services/common.service';
import { setFocusToElement } from 'projects/fibi/src/app/common/utilities/custom-utilities';

import { ExpandedActionListService } from './expanded-action-list.service';
import { getTimeInterval, parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { fadeInOutHeight } from 'projects/fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-expanded-action-list',
    templateUrl: './expanded-action-list.component.html',
    styleUrls: ['./expanded-action-list.component.scss'],
    animations: [fadeInOutHeight]
})
export class ExpandedActionListComponent implements OnInit, OnDestroy {

    inboxObject: any = {
        moduleCode: null
    };
    inboxDetails: any = [];
    testInboxDeatils: any = [
        {
            inbox: {
                message: {
                    description: "Disclosure Approved",
                    disclosureId:"66",
                    Proposal:" #25849 - Start-up Research Grant (SRG) scheme"
                },
                messageTypeCode: '123',
                class: "fresher",
                arrivalDate: "31/01/2015"
            },

        },
        // {
        //     inbox: {
        //         message: {
        //             description: "Disclosure Submitted for Review",
        //             disclosureId:"3",
        //             Proposal:" #25849 - Start-up Research Grant (SRG) scheme"
        //         },
        //         messageTypeCode: '123',
        //         arrivalDate:'21/09/2010'
        //     },

        // },
        {
            inbox: {
                message: {
                    description: "Disclosure Submitted for Review"                 ,
                    disclosureId:"16",
                    Proposal:" #25849 - UV induced Cell Death in Plants- An overview"
                },
                messageTypeCode: '123',
                arrivalDate: "01/02/2004"
            },

        },
        {
            inbox: {
                message: {
                    description: "Management Plan uploaded ",
                    disclosureId:"62",
                    Proposal:" #25849 - Start-up Research Grant (SRG) scheme"
                },
                messageTypeCode: '123',
                arrivalDate: "03/02/2010"
            },

        },
        {
            inbox: {
                message: {
                    description: "Potential Conflict Identified",
                    disclosureId:"61",
                   
                },
                messageTypeCode: '123',
                arrivalDate: "04/02/2002"

            },

        }
        
    ];
    $subscriptions: Subscription[] = [];
    modulePath = Object.assign({}, Constants.paths);
    viewInboxSearch = false;
    inboxTab = 'PENDING';
    datePlaceHolder = DATE_PLACEHOLDER;
    setFocusToElement = setFocusToElement;
    moduleList: any = [];
    isInboxInfo = true;
    getTimeInterval = getTimeInterval;
    isSaving = false;

    constructor(private _actionList: ExpandedActionListService, private _router: Router,
        public _commonService: CommonService) { }

    ngOnInit() {
        this.clearInboxSearchField();
        this.getActionList(false);
        
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getActionList(type) {
        if (!this.isSaving) {
            this.isSaving = true;
            this.inboxObject.toPersonId = this._commonService.getCurrentUserDetail('personId');
            this.inboxObject.isViewAll = 'N';
            this.inboxObject.processed = type;
            this.inboxObject.fromDate = parseDateWithoutTimestamp(this.inboxObject.fromDate);
            this.inboxObject.toDate = parseDateWithoutTimestamp(this.inboxObject.toDate);
            this.$subscriptions.push(this._actionList.getActionList(this.inboxObject).subscribe((data: any) => {
                this.inboxDetails = data.inboxDetails;
                this.moduleList = data.modules;
                this.isSaving = false;
                this.inboxDetails.forEach(element => {
                    Object.keys(this.modulePath).forEach(key => {
                        if (key === this.getModulePathKey(element)) {
                            element.class = this.modulePath[key].class;
                            element.name = this.modulePath[key].name;
                        }
                    });
                });
            }, err => { this.isSaving = false; }));
        }
    }

    getModulePathKey(el) {
        return el.moduleCode !== 1 ? el.moduleCode.toString() : el.moduleCode.toString() + this.getAwardSubmoduleCode(el);
    }

    getAwardSubmoduleCode(el) {
        return el.subModuleCode ? el.subModuleCode.toString() : '0';
    }

    getSubModulePath(el) {
        return this.modulePath[this.getModulePathKey(el)].subPath ? this.modulePath[this.getModulePathKey(el)].subPath : '';
    }

    getSubModuleKey(el) {
        return this.modulePath[this.getModulePathKey(el)].subPath ? el.subModuleItemKey : '';
    }

    goToActionPath(inbox, i) {
        if (inbox.moduleCode.toString() === '3') {
            if (inbox.messageTypeCode === '105') {
                localStorage.setItem('currentTab', 'PROPOSAL_HOME');
            } else if (['133', '134'].includes(inbox.messageTypeCode)) {
                localStorage.setItem('currentTab', 'CERTIFICATION');
                this._router.navigate(['fibi/proposal/certification'], { queryParams: { proposalId: inbox.moduleItemKey } });
                return;
            } else {
                localStorage.setItem('currentTab', 'PROPOSAL_REVIEW');
                this._router.navigate(['fibi/proposal/summary'], { queryParams: { proposalId: inbox.moduleItemKey } });
                return;
            }
        }
        window.open(window.location.origin + window.location.pathname + this.modulePath[this.getModulePathKey(inbox)].path
            + inbox.moduleItemKey + this.getSubModulePath(inbox) + this.getSubModuleKey(inbox), '_self');
    }

    markAsRead(id) {
        this.$subscriptions.push(this._actionList.openUnreadInbox(id).subscribe(data => { }));
    }

    clearInboxSearchField() {
        this.inboxObject.moduleCode = null;
    }

    getInboxTab() {
        this.inboxTab === 'PENDING' ? this.getActionList(false) : this.getActionList(true);
        this.viewInboxSearch = false;
        // this.clearInboxSearchField();
    }
    addingDataToDummyArray() {
        this.testInboxDeatils = [
            {
                inbox: {
                    message: {
                        description: "Management Plan uploaded"
                    }
                },

            },
            {
                inbox: {
                    message: {
                        description: "Management Plan uploaded"
                    }
                },

            },
        ]
    }

}


