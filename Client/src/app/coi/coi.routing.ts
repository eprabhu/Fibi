import { Routes, RouterModule } from '@angular/router';

import { CoiComponent } from './coi.component';
import { ResolveServiceService } from './services/resolve-service.service';
import { RouterGuardService } from './services/router-guard.service';

const routes: Routes = [
    {
        path: '', component: CoiComponent, canActivate: [ResolveServiceService],
        children: [
            { path: '', redirectTo: 'summary', pathMatch: 'full' },
            {
                path: 'screening-questionnaire',
                loadChildren: () => import('./screening-questionnaire/screening-questionnaire.module')
                .then(m => m.ScreeningQuestionnaireModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'sfi',
                loadChildren: () => import('./sfi/sfi.module')
                .then(m => m.SfiModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'relationship',
                loadChildren: () => import('./relationship/relationship.module')
                .then(m => m.RelationshipModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'certify',
                loadChildren: () => import('./certify/certify.module')
                .then(m => m.CertifyModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'attachment',
                loadChildren: () => import('./attachment/attachment.module')
                .then(m => m.AttachmentModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'review',
                loadChildren: () => import('./review/review.module')
                .then(m => m.ReviewModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'summary',
                loadChildren: () => import('./summary/summary.module')
                .then(m => m.SummaryModule),
                canActivate: [RouterGuardService]
            },
            {
                path: 'history',
                loadChildren: () => import('./history/history.module')
                .then(m => m.HistoryModule),
                canActivate: [RouterGuardService]
            },
        ]
    },
];

export const CoiRoutes = RouterModule.forChild(routes);
