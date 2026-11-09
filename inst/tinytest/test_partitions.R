# Partition filter template
w <- ora:::ora_partition_where()
expect_true(grepl("{dict}_indexes", w, fixed = TRUE))
expect_equal(lengths(regmatches(w, gregexpr("?", w, fixed = TRUE))), 4L)
rendered <- ora:::ora_render_dict(w, "all")
expect_true(grepl("all_indexes", rendered, fixed = TRUE))
expect_false(grepl(";", rendered, fixed = TRUE))
