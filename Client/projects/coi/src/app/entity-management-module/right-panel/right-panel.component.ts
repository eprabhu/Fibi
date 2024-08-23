import { Component } from '@angular/core';
import { AttachmentTab, ComplianceTab, OverviewTabSection, SponsorTabSection, SubawardOrganisationTab } from '../shared/entity-constants';
import { jumpToSection } from '../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { Router } from '@angular/router';

@Component({
  selector: 'app-right-panel',
  templateUrl: './right-panel.component.html',
  styleUrls: ['./right-panel.component.scss']
})
export class RightPanelComponent {

    currentTab: 'OVERVIEW' | 'SPONSOR' | 'SUBAWARD' | 'COMPLIANCE' | 'ATTACHMENTS' | '';
    sectionDetails: [];
    selectedSectionId: any;
    $subscriptions: Subscription[] = [];

    constructor(private _router: Router) {}

    ngOnInit() {
        this.routerEventSubscription();
    }

    getCurrentTab(currentURL): any {
        if(currentURL.includes('entity-overview')) {
            return 'OVERVIEW';
        } else if (currentURL.includes('entity-sponsor')) {
            return 'SPONSOR';
        } else if (currentURL.includes('entity-subaward')) {
            return 'SUBAWARD';
        } else if (currentURL.includes('entity-compliance')) {
            return 'COMPLIANCE';
        } else if (currentURL.includes('entity-attachments')) {
            return 'ATTACHMENTS';
        } else {
            return '';
        }
    }

    getSectionDetails() {
        this.sectionDetails = [];
        switch(this.currentTab) {
            case 'OVERVIEW': {
                this.sectionDetails = this.getArray(OverviewTabSection);
                break;
            }
            case 'SPONSOR': {
                this.sectionDetails = this.getArray(SponsorTabSection);
                break;
            }
            case 'SUBAWARD': {
                this.sectionDetails = this.getArray(SubawardOrganisationTab);
                break;
            }
            case 'COMPLIANCE': {
                this.sectionDetails = this.getArray(ComplianceTab);
                break;
            }
            case 'ATTACHMENTS': {
                this.sectionDetails = this.getArray(AttachmentTab);
                break;
            }
            default: {
                this.sectionDetails = [];
                break;
            }
        }
    }

    scrollToSelectedSection(section) {
        this.selectedSectionId = section.sectionId;
        const offset = (document.getElementById('COI-DISCLOSURE-HEADER')?.getBoundingClientRect().height + 100);
        jumpToSection(this.selectedSectionId, offset);
        // this.windowScroll(this.selectedSectionId);
    }

    windowScroll(scrollTo: string) {
        const ELEMENT: HTMLElement = document.getElementById(scrollTo);
        const offsetFromHeader = document.getElementById('COI-DISCLOSURE-HEADER')?.clientHeight + 50;
        const sectionHeight = ELEMENT.offsetTop - offsetFromHeader;
        window.scrollTo({ behavior: 'smooth', top: sectionHeight });
    }

    getArray(map): any {
        let sectionName = [];
        map.forEach((value) => {
            sectionName.push(value);
        });
        return sectionName;
    }

    routerEventSubscription() {
        this.$subscriptions.push(this._router.events.subscribe(event => {
            this.currentTab  = this.getCurrentTab(window.location.href);
            this.getSectionDetails();
        }));
      }

}
