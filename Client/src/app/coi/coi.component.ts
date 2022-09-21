import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { environment } from '../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { subscriptionHandler } from '../common/utilities/subscription-handler';
import { COI, CommentConfiguration } from './coi-interface';
import { CoiService } from './services/coi.service';

import { DataStoreService } from './services/data-store.service';

declare var $: any;

@Component({
    selector: 'app-coi',
    templateUrl: './coi.component.html',
    styleUrls: ['./coi.component.css']
})
export class CoiComponent implements OnInit, OnDestroy {

    @ViewChild('moreOptionsBtn', { static: false }) moreOptions: ElementRef;

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure', 'person', 'numberOfSFI', 'proposalIdlinkedInDisclosure'];
    coiData: COI = new COI();
    isCOIAdministrator = false;
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    isShowCountModal: any;
    selectedModuleCode: number;
    isShowMoreOptions = false;

    constructor(
        private _dataStore: DataStoreService,
        private _route: ActivatedRoute,
        private _router: Router,
        private _commonService: CommonService,
        public _coiService: CoiService
    ) {
        document.addEventListener('mouseup', this.offClickHandler.bind(this));
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.checkIfUserIsAdmin();
        this._coiService.stepTabName = this._router.url.split('/')[3].split('?')[0];
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    async checkIfUserIsAdmin() {
        this.isCOIAdministrator = await this._commonService.checkPermissionAllowed('COI_ADMINISTRATOR');
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    navigateToList() {
        const LIST_URL = this.isCOIAdministrator ? '/fibi/dashboard/coi-admin-list' : '/fibi/dashboard/coi-list';
        this._router.navigateByUrl(LIST_URL);
    }

    private getDataFromStore() {
        this.coiData = this._dataStore.getData();
        this.commentConfiguration.disclosureId = this.coiData.coiDisclosure.disclosureId;
        this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (!MODULE_ID) {
                this._router.navigate([], {
                    queryParams: {
                        disclosureId: this.coiData.coiDisclosure.disclosureId
                    },
                    queryParamsHandling: 'merge',
                });
            }
        });
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            default: return 'danger';
        }
    }
    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2':
            case '4':
            case '5':
                return 'info';
            case '3': case '6': return 'success';
            default: return 'danger';
        }
    }
    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case 2: return 'success';
            case 3: return 'danger';
            default: return 'info';
        }
    }

    modifyReviewComment() {
        this._coiService.triggerCommentModal(this.commentConfiguration);
    }

    closeModal(event) {
		this.isShowCountModal = event;
	}

    setSelectedModuleCode(moduleName) {
		switch (moduleName) {
			case 'sfi': {
                this.selectedModuleCode = 8;
				break;
			}
			case 'award': {
				this.selectedModuleCode = 1;
				break;
			}
			case 'proposal': {
				this.selectedModuleCode = 3;
				break;
			}
			case 'disclosure': {
				this.selectedModuleCode = 101;
				break;
			}
			default: this.selectedModuleCode = 0;
		}
		this.isShowCountModal = true;
	}

    showInfo() {
        switch (this._coiService.stepTabName) {
            case 'sfi': {
                this._coiService.isShowSFIInfo = !this._coiService.isShowSFIInfo;
                break;
            }
            case 'certify': {
                this._coiService.isShowCertifyInfo = !this._coiService.isShowCertifyInfo;
                break;
            }case 'history': {
                this._coiService.isShowHistoryInfo = !this._coiService.isShowHistoryInfo;
                break;
            }case 'relationship': {
                this._coiService.isShowInfo = !this._coiService.isShowInfo;
                break;
            }case 'attachment': {
                this._coiService.isShowAttachmentInfo = !this._coiService.isShowAttachmentInfo;
                break;
            }
            default: break;
          }
    }

    showInfoTooltip() {
        switch (this._coiService.stepTabName) {
            case 'sfi':  return  this._coiService.isShowSFIInfo;
            case 'certify': return this._coiService.isShowCertifyInfo;
            case 'history': return this._coiService.isShowHistoryInfo;
            case 'relationship': return this._coiService.isShowInfo;
            case 'attachment': return this._coiService.isShowAttachmentInfo;
            default: return false;
        }
    }

    completeDisclosureReview() {
        this.$subscriptions.push(this._coiService.completeDisclosureReview(this.coiData.coiDisclosure.disclosureId)
            .subscribe((res: any) => {
                this.updateDisclosureReviewStatus(res);
            }, _err => {
                _err.error.text === 'REVIEW_STATUS_NOT_COMPLETE' ? $('#completeReviewErrorModal').modal('show') :
                    this._commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
            }));
    }

    updateDisclosureReviewStatus(res) {
        this.coiData.coiDisclosure = JSON.parse(JSON.stringify(res));
        this._dataStore.updateStore(['coiDisclosure'], this.coiData);
        this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
    }

    offClickHandler(event: any) {
        if (this.moreOptions) {
          if (!this.moreOptions.nativeElement.contains(event.target)) {
            this.isShowMoreOptions = false;
          }
        }
    }

    getStepName() {
        switch (this._coiService.stepTabName) {
            case 'screening-questionnaire' : return `<strong>Step 1 of 4</strong> : Screening Questionnaire`;
            case 'sfi' : return `<strong>Step 2 of 4</strong> : Significant Financial Interests`;
            case 'relationship' : return `<strong>Step 3 of 4</strong> : Relationships`;
            case 'certify' : return `<strong>Step 4 of 4</strong> : Certify`;
            default: return '';
        }
    }
}
