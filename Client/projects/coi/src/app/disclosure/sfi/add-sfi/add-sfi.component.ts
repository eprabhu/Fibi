import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { ActivityService } from '../../../../../../fibi/src/app/agreement/agreement-shared/activity-track/activity.service';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { SfiService } from '../../../disclosure/sfi/sfi.service';
import { environment } from '../../../../environments/environment';
import { CommonService } from '../../../common/services/common.service';

@Component({
    selector: 'app-add-sfi',
    templateUrl: './add-sfi.component.html',
    styleUrls: ['./add-sfi.component.scss'],
    providers: [ActivityService],
    animations: [slideHorizontal]
})
export class AddSfiComponent implements OnInit {

    isSaving = false;
    scrollHeight: number;
    deployMap = environment.deployUrl;
    @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
    isAddAttachment = false;
    isAddAssignee = false;
    dateTime: string;
    isReadMore: false;

    constructor(public sfiService: SfiService, public _commonService: CommonService) { }

    ngOnInit() {
        this.showSfiNavBar();
    }

    hideSfiNavBar() {
        if (!this.isSaving) {
            this.sfiService.isShowSfiNavBar = false;
            this.showSfiNavBar();
        }
    }

    showSfiNavBar() {
        if (this.sfiService.isShowSfiNavBar) {
            this.scrollHeight = document.documentElement.scrollTop;
            document.documentElement.classList.add('cdk-global-scrollblock');
            document.documentElement.style.top = - this.scrollHeight + 'px';
        }
    }

}
