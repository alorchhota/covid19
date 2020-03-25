library(ioutil)
library(miscutil)
library(ggplot2)
library(GGally)

expr_fn = "/work-zfs/abattle4/ashis/progdata/scnet/gtex_v8_net/v8_cis_eqtl_expr_corrected_sym/Small_Intestine_Terminal_Ileum.v8.corrected.VAR.1500.txt"
out_dir = "/work-zfs/abattle4/ashis/prog/covid19/results/ace2_coexpr"
n_top = 50
method = "pearson"
target_gene = "ACE2" # "ACE2" "TMPRSS2"

### read data
expr_df = read_df(expr_fn)
net = cor(t(expr_df), method = method)

### get top correlated genes
ace2_bg_genes = setdiff(colnames(net), target_gene)
ace2_edges = net[target_gene, ace2_bg_genes]
sorted_ace2_edges = ace2_edges[order(abs(ace2_edges), decreasing=T)]
top_corr_df = data.frame(gene1 = target_gene, gene2 = names(sorted_ace2_edges)[1:n_top], r = as.numeric(sorted_ace2_edges[1:n_top]), stringsAsFactors = F)
write_df(top_corr_df, file=sprintf("%s/%s_top_corr_%s.txt", out_dir, target_gene, method), row.names=F, col.names=F)

### correlation plot
top_corr_plt_df = top_corr_df
top_corr_plt_df$gene2 = factor(top_corr_plt_df$gene2, levels = top_corr_plt_df$gene2)
top_corr_plt_df$status = c("Positive correlation", "Negative correlation")[as.integer(top_corr_plt_df$r < 0)+1]
top_corr_plt_df$r = abs(top_corr_plt_df$r)

scatter_plt_df = NULL
for( g in names(sorted_ace2_edges[1:9])){
  df0 = data.frame(ACE2_expression = as.numeric(expr_df[target_gene,]),
                   gene_expression = as.numeric(expr_df[g,]),
                   gene = g,
                   stringsAsFactors = F)
  scatter_plt_df = rbind(scatter_plt_df, df0)
}
scatter_plt_df$gene = factor(scatter_plt_df$gene, levels = names(sorted_ace2_edges)[1:9])

top_corr_plt_fn = sprintf("%s/%s_top_corr_%s.pdf", out_dir, target_gene, method)
pdf(top_corr_plt_fn)
ggplot(top_corr_plt_df, aes(x=gene2, y=r, fill=status))+
  geom_bar(stat="identity", color="black")+
  scale_fill_brewer(palette="Blues") +
  theme_bw() +
  theme(axis.title.y = element_blank()) + 
  ylab("|r|") + 
  theme(legend.position="top", legend.title = element_blank()) +
  scale_x_discrete(limits = rev(levels(top_corr_plt_df$gene2))) +
  coord_flip() 

ggplot(scatter_plt_df, aes(x=ACE2_expression, y=gene_expression)) +
  geom_point(shape=1) + 
  geom_smooth(method = "lm", formula = y ~ x) +
  xlab(sprintf("%s expression", target_gene)) +
  ylab(sprintf("Normalized gene expression")) +
  facet_wrap( ~ gene, ncol=3)

dev.off()




