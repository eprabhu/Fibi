import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { DashboardComponent } from './dashboard.component';
import { AuthGuard } from '../common/services/auth-guard.service';

const routes: Routes = [{
    path: '', component: DashboardComponent,
    children: [
        {path: '', redirectTo: 'researchSummary', pathMatch: 'full'},
        {
            path: 'researchSummary',
            loadChildren: () => import('../research-summary-widgets/research-summary-widgets.module')
                .then(m => m.ResearchSummaryWidgetsModule), canActivate: [AuthGuard]
        },
       
        {path: 'report', loadChildren: () => import('../report/report.module').then(m => m.ReportModule), canActivate: [AuthGuard]},
        {
            path: 'progressReportList',
            loadChildren: () => import('../progress-report/progress-report-list/progress-report-list.module')
                .then(m => m.ProgressReportListModule), canActivate: [AuthGuard]
        },
        {
            path: 'coi-list',
            loadChildren: () => import('../coi/coi-dashboard/coi-dashboard.module')
                .then(m => m.CoiDashboardModule), canActivate: [AuthGuard]
        },
        {
            path: 'coi-admin-list',
            loadChildren: () => import('../coi/coi-admin-dashboard/coi-admin-dashboard.module')
                .then(m => m.CoiAdminDashboardModule), canActivate: [AuthGuard]
        },
    ]
},
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class DashboardRoutingModule {
}
