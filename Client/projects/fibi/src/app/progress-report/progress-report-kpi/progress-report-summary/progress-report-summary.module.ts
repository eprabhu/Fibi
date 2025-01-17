import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ProgressReportSummaryComponent} from './progress-report-summary.component';
import {RouterModule, Routes} from '@angular/router';
import {SharedModule} from '../../../shared/shared.module';

const routes: Routes = [
    {path: '', component: ProgressReportSummaryComponent}
];

@NgModule({
    declarations: [ProgressReportSummaryComponent],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedModule
    ]
})
export class ProgressReportSummaryModule {
}
